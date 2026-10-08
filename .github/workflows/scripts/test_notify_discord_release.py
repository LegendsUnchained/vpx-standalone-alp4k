import importlib.util
import os
import unittest

_spec = importlib.util.spec_from_file_location(
    "notify", os.path.join(os.path.dirname(__file__), "notify-discord-release.py"))
notify = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(notify)

CAT = "https://vpxtablemanager.com/catalog/#table="
TST = "https://vpxtablemanager.com/testers/#table="


def body(new, updated, base=CAT):
    lines = []
    if new:
        lines.append("## Newly added tables")
        lines += [f"- [New Table {i} (Stern 2001)]({base}vpx-new{i}) (`vpx-new{i}`)" for i in range(new)]
    if updated:
        lines.append("## Updated tables:")
        lines += [f"- [Updated Table {i} (Bally 1980)]({base}vpx-upd{i}) (`vpx-upd{i}`)" for i in range(updated)]
    return "\n".join(lines)


def release(text, tag="pre-release"):
    return {"tag_name": tag, "html_url": "https://github.com/x/y/releases/tag/" + tag,
            "published_at": "2026-10-08T12:00:00Z", "body": text}


class TestingTests(unittest.TestCase):
    def test_header_and_links(self):
        [msg] = notify.payloads(release(body(2, 3)), "testing")
        e = msg["embeds"][0]
        self.assertEqual(e["title"], "Wizard Table Pre-Release")
        self.assertEqual(e["url"], "https://vpxtablemanager.com/testers/")
        self.assertTrue(e["description"].startswith(
            "**DO NOT FORWARD THIS ANNOUNCEMENT**\n\nPlease test the following tables and "
            "sign-off after testing at https://vpxtablemanager.com/testers:"))
        self.assertIn("### New tables (2)", e["description"])
        self.assertIn("### Updated tables (3)", e["description"])
        # Every table, new ones included, links to its testers-page card.
        self.assertIn(f"]({TST}vpx-new0)", e["description"])
        self.assertIn(f"]({TST}vpx-upd2)", e["description"])
        self.assertNotIn(CAT, e["description"])
        self.assertNotIn("Wizard Wednesday", e["description"])
        self.assertNotIn("more in the", e["description"])

    def test_long_announcements_continue_rather_than_truncate(self):
        msgs = notify.payloads(release(body(40, 160)), "testing")
        self.assertGreater(len(msgs), 1)
        text = "\n".join(m["embeds"][0]["description"] for m in msgs)
        for i in range(40):
            self.assertEqual(text.count(f"{TST}vpx-new{i})"), 1, f"vpx-new{i}")
        for i in range(160):
            self.assertEqual(text.count(f"{TST}vpx-upd{i})"), 1, f"vpx-upd{i}")
        self.assertEqual(msgs[0]["embeds"][0]["title"], "Wizard Table Pre-Release")
        for n, m in enumerate(msgs):
            e = m["embeds"][0]
            if n:
                self.assertEqual(e["title"], "Wizard Table Pre-Release (continued)")
                self.assertTrue(e["description"].startswith("### "), e["description"][:40])
                self.assertNotIn("image", e)
            self.assertTrue(e["footer"]["text"].endswith(f"{n + 1} of {len(msgs)}"))
            total = sum(notify.length(e.get(k, "")) for k in ("title", "description")) + \
                notify.length(e["footer"]["text"])
            self.assertLessEqual(notify.length(e["description"]), notify.DESCRIPTION_MAX)
            self.assertLessEqual(total, notify.EMBED_MAX)
            self.assertEqual(len(m["embeds"]), 1)

    def test_nothing_to_test(self):
        [msg] = notify.payloads(release(""), "testing")
        self.assertIn("No new or updated tables to test.", msg["embeds"][0]["description"])


class StableTests(unittest.TestCase):
    def test_stable_is_one_message_with_catalog_links(self):
        msgs = notify.payloads(release(body(40, 160), tag="v2.0.15"), "stable")
        self.assertEqual(len(msgs), 1)
        e = msgs[0]["embeds"][0]
        self.assertEqual(e["title"], "Wizard Table Release - v2.0.15")
        self.assertIn("more in the [Table Manager Catalog]", e["description"])
        self.assertIn(CAT, e["description"])
        self.assertLessEqual(notify.length(e["description"]), notify.DESCRIPTION_MAX)


if __name__ == "__main__":
    unittest.main()
