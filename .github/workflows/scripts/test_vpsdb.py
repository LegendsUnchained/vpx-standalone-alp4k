import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch

import vpsdb


class DecodeArchivePasswordsTest(unittest.TestCase):
    def test_decodes_password_list_in_order(self):
        self.assertEqual(
            vpsdb.decode_archive_passwords(["d3Jvbmc=", "anBncg=="]),
            ["wrong", "jpgr"],
        )

    def test_accepts_legacy_scalar_and_deduplicates(self):
        self.assertEqual(vpsdb.decode_archive_passwords("anBncg=="), ["jpgr"])
        self.assertEqual(
            vpsdb.decode_archive_passwords(["anBncg==", "anBncg=="]),
            ["jpgr"],
        )

    def test_absent_value_stays_absent(self):
        self.assertIsNone(vpsdb.decode_archive_passwords(None))

    def test_rejects_invalid_base64(self):
        with self.assertRaisesRegex(ValueError, "not valid Base64"):
            vpsdb.decode_archive_passwords(["not-base64"])

    def test_get_table_meta_emits_decoded_archive_passwords(self):
        class FakeVPSDB:
            def get_tablefile_by_id(self, vps_id):
                if vps_id != "fixture-vpx":
                    raise AssertionError(f"unexpected VPS id: {vps_id}")
                return {
                    "authors": ["Fixture Author"],
                    "urls": [{"url": "https://example.invalid/table.vpx"}],
                    "version": "1.0",
                }

        with TemporaryDirectory() as temp_dir:
            table_dir = Path(temp_dir) / "vpx-password-fixture"
            table_dir.mkdir()
            table_yml = table_dir / "table.yml"
            table_yml.write_text(
                "vpxVPSId: fixture-vpx\n"
                'vpxChecksum: "00000000000000000000000000000000"\n'
                "vpxMagic:\n"
                "  - d3Jvbmc=\n"
                "  - anBncg==\n"
                "postInstallRename:\n"
                '  - source: "Music"\n'
                '    destination: "assets/Music"\n'
            )

            with patch.object(vpsdb, "VPSDB", return_value=FakeVPSDB()):
                meta = vpsdb.get_table_meta([str(table_yml)])

        self.assertEqual(
            meta["vpx-password-fixture"]["archivePassword"],
            ["wrong", "jpgr"],
        )
        self.assertEqual(
            meta["vpx-password-fixture"]["postInstallRename"],
            [{"source": "Music", "destination": "assets/Music"}],
        )


class PostInstallRenameTest(unittest.TestCase):
    def test_normalizes_ordered_relative_file_and_folder_moves(self):
        value = [
            {"source": "file1.ext", "destination": "renamed/file.ext"},
            {"source": "folder1", "destination": "assets/renamedFolder"},
        ]
        self.assertEqual(vpsdb.normalize_post_install_renames(value), value)

    def test_absent_and_empty_values_stay_absent(self):
        self.assertIsNone(vpsdb.normalize_post_install_renames(None))
        self.assertIsNone(vpsdb.normalize_post_install_renames([]))

    def test_rejects_invalid_rule_shapes(self):
        invalid_values = [
            {"source": "file", "destination": "renamed"},
            ["file", "renamed"],
            [{"source": "file"}],
            [{"source": "file", "destination": "renamed", "extra": True}],
        ]
        for value in invalid_values:
            with self.subTest(value=value):
                with self.assertRaises(ValueError):
                    vpsdb.normalize_post_install_renames(value)

    def test_rejects_unsafe_paths_and_globs(self):
        invalid_paths = [
            "/absolute",
            "folder/",
            "../outside",
            "folder/../../outside",
            r"folder\windows",
            "*.vpx",
            "folder/[ab]",
            ".",
        ]
        for path in invalid_paths:
            for field in ("source", "destination"):
                with self.subTest(path=path, field=field):
                    rule = {"source": "source", "destination": "renamed"}
                    rule[field] = path
                    with self.assertRaises(ValueError):
                        vpsdb.normalize_post_install_renames([rule])

    def test_rejects_same_normalized_path(self):
        with self.assertRaisesRegex(ValueError, "same path"):
            vpsdb.normalize_post_install_renames(
                [{"source": "folder/../file", "destination": "file"}]
            )


if __name__ == "__main__":
    unittest.main()
