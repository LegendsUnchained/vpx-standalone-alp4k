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
            )

            with patch.object(vpsdb, "VPSDB", return_value=FakeVPSDB()):
                meta = vpsdb.get_table_meta([str(table_yml)])

        self.assertEqual(
            meta["vpx-password-fixture"]["archivePassword"],
            ["wrong", "jpgr"],
        )


if __name__ == "__main__":
    unittest.main()
