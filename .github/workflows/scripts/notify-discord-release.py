"""Post a release's new and updated tables to Discord as embeds (stdlib only).

    notify-discord-release.py [--kind stable|testing] [--dry-run] RELEASE_JSON

RELEASE_JSON is the release object from the GitHub API. The webhook comes from
DISCORD_WEBHOOK_URL; --dry-run prints the payloads instead of sending them.

stable is the promotion to a published release (promote-release.yml): one
embed, led by a greeting ("Happy Wizard Wednesday!" when the release went out
on a Wednesday, US Eastern time, and a wistful version naming the actual day
otherwise). Past Discord's limits it ends with "…and N more in the Table
Manager Catalog", where every promoted table can be found.

testing is tables arriving in the rolling pre-release (sync-prerelease.yml),
for testers only. Its tables are not in the catalog yet, so each one links to
its card on the testers page, where it is signed off, and nothing is cut: what
does not fit in one message goes out in further "(continued)" messages.
"""

import argparse
import datetime
import json
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from zoneinfo import ZoneInfo

IMAGE = "https://vpxsmedia.legendsunchained.com/discord_embed_tm.png"
CATALOG = "https://vpxtablemanager.com/catalog"
TESTERS = "https://vpxtablemanager.com/testers"
CATALOG_TABLE = CATALOG + "/#table="
TESTERS_TABLE = TESTERS + "/#table="

KINDS = {
    "stable": {"color": 0x6BD3F1, "footer": "Stable release"},
    "testing": {"color": 0xF5A623, "footer": "Testing release · beta testers"},
}

# "Wizard Wednesday" is Wednesday on US Eastern time, EDT or EST as the date has it.
EASTERN = ZoneInfo("America/New_York")
DAYS = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
INTRO = "The Wizard team is proud to present the following tables for your enjoyment:"

TESTING_TITLE = "Wizard Table Pre-Release"
TESTING_INTRO = ("**DO NOT FORWARD THIS ANNOUNCEMENT**\n\n"
                 f"Please test the following tables and sign-off after testing at {TESTERS}:")

SECTIONS = (("newly added tables", "New tables"), ("updated tables", "Updated tables"))

# Discord's embed limits, counted in UTF-16 code units as Discord counts them.
TITLE_MAX, DESCRIPTION_MAX, EMBED_MAX = 256, 4096, 6000


def length(text):
    return len(text.encode("utf-16-le")) // 2


def sections(body):
    """The table entries under each heading, in order, one per line."""
    found = {title: [] for _, title in SECTIONS}
    current = None
    for line in body.splitlines():
        if line.startswith("## "):
            heading = line[3:].strip().rstrip(":").lower()
            current = dict(SECTIONS).get(heading)
        elif current and line.strip().startswith(("- ", "* ")):
            found[current].append(tidy(line.strip()))
    return found


def tidy(entry):
    """Drop the trailing (`slug`): the catalog link already identifies the table."""
    return re.sub(r"\s*\(`[^`]*`\)\s*$", "", entry)


def greeting(release):
    """Happy Wizard Wednesday, or a lament for whatever day it actually is.

    The day is the release's publish time on US Eastern time.
    """
    stamp = release.get("published_at") or release.get("created_at")
    when = (datetime.datetime.fromisoformat(stamp.replace("Z", "+00:00"))
            if stamp else datetime.datetime.now(datetime.timezone.utc))
    # A fixed list, not strftime("%A"), which follows the runner's locale.
    day = DAYS[when.astimezone(EASTERN).weekday()]
    if day == "Wednesday":
        return "Happy Wizard Wednesday!"
    return f"Happy Wizard... {day}? It just doesn't have the same ring to it..."


def description(found, kept, hello):
    parts = [hello, INTRO]
    for title, entries in found.items():
        if not entries:
            continue
        shown = entries[: kept[title]]
        block = f"### {title} ({len(entries)})\n" + "\n".join(shown)
        if len(shown) < len(entries):
            block += f"\n…and {len(entries) - len(shown)} more in the [Table Manager Catalog]({CATALOG})"
        parts.append(block)
    if len(parts) == 2:
        parts.append("No new or updated tables in this release.")
    return "\n\n".join(parts)


def embed(release, kind):
    style = KINDS[kind]
    tag = release["tag_name"].strip()
    # Stable releases are titled with their version. The rolling pre-release
    # has none (its tag is just `pre-release`), and the footer already says it
    # is a testing release, so its title stands alone.
    if re.match(r"^v?\d", tag):
        title = f"Wizard Table Release - {tag if tag.startswith('v') else 'v' + tag}"
    else:
        title = "Wizard Table Release"
    title = title[:TITLE_MAX]
    footer = style["footer"]
    found = sections(release.get("body") or "")
    hello = greeting(release)
    kept = {t: len(e) for t, e in found.items()}
    limit = min(DESCRIPTION_MAX, EMBED_MAX - length(title) - length(footer))
    # Drop whole entries, from whichever section is longer, until it fits.
    while length(desc := description(found, kept, hello)) > limit:
        longest = max(kept, key=lambda t: kept[t])
        if kept[longest] == 0:
            raise ValueError("the release notes cannot fit Discord's embed limit")
        kept[longest] -= 1
    out = {
        "title": title,
        "url": release["html_url"],
        "description": desc,
        "color": style["color"],
        "image": {"url": IMAGE},
        "footer": {"text": footer},
    }
    if release.get("published_at"):
        out["timestamp"] = release["published_at"]
    return out


def testers_link(entry):
    """Point a catalog link at the table's card on the testers page instead."""
    return entry.replace("(" + CATALOG_TABLE, "(" + TESTERS_TABLE)


def testing_embeds(release):
    """The pre-release announcement, packed into as many embeds as it takes.

    Entries are never dropped: when the next one would overflow Discord's
    limit, the embed is closed and a "(continued)" one starts, repeating the
    section heading so every part reads on its own.
    """
    style = KINDS["testing"]
    footer = style["footer"]
    found = {t: [testers_link(e) for e in entries]
             for t, entries in sections(release.get("body") or "").items()}
    title_more = f"{TESTING_TITLE} (continued)"
    limit = min(DESCRIPTION_MAX, EMBED_MAX - length(title_more) - length(footer))

    parts, lines = [], [TESTING_INTRO]
    def close():
        parts.append("\n".join(lines).strip())
    for title, entries in found.items():
        if not entries:
            continue
        heading = f"\n### {title} ({len(entries)})"
        if length("\n".join(lines + [heading, entries[0]])) > limit:
            close()
            lines = []
        lines.append(heading)
        for entry in entries:
            if length(entry) + length(heading) > limit:
                raise ValueError("a single table entry cannot fit Discord's embed limit")
            if length("\n".join(lines + [entry])) > limit:
                close()
                lines = [f"### {title} (continued)"]
            lines.append(entry)
    if len(lines) > 1 or not parts:
        if len(lines) == 1 and lines[0] == TESTING_INTRO:
            lines.append("\nNo new or updated tables to test.")
        close()

    out = []
    for i, desc in enumerate(parts):
        e = {
            "title": TESTING_TITLE if i == 0 else title_more,
            "url": TESTERS + "/",
            "description": desc,
            "color": style["color"],
            "footer": {"text": footer if len(parts) == 1 else f"{footer} · {i + 1} of {len(parts)}"},
        }
        if i == 0:
            e["image"] = {"url": IMAGE}
        if release.get("published_at"):
            e["timestamp"] = release["published_at"]
        out.append(e)
    return out


def payloads(release, kind):
    """The webhook messages to send, in order: one embed each."""
    embeds = testing_embeds(release) if kind == "testing" else [embed(release, kind)]
    return [{"embeds": [e], "allowed_mentions": {"parse": []}} for e in embeds]


def payload(release, kind):
    """The first (for stable, the only) message."""
    return payloads(release, kind)[0]


def send(url, body):
    parts = urllib.parse.urlsplit(url)
    query = dict(urllib.parse.parse_qsl(parts.query))
    query["wait"] = "true"
    url = urllib.parse.urlunsplit(parts._replace(query=urllib.parse.urlencode(query)))
    request = urllib.request.Request(url, data=json.dumps(body).encode(), headers={
        "Content-Type": "application/json", "User-Agent": "ReleaseNotifier/2.0"})
    for attempt in range(5):
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                response.read()
            return
        except urllib.error.HTTPError as error:
            if error.code == 429 and attempt < 4:
                time.sleep(float(json.loads(error.read())["retry_after"]))
                continue
            detail = error.read().decode(errors="replace")[:300]
            # Never print the URL: it contains the webhook credential.
            raise RuntimeError(f"Discord returned HTTP {error.code}: {detail}") from None
        except urllib.error.URLError:
            raise RuntimeError("Could not connect to Discord") from None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("release_json")
    parser.add_argument("--kind", choices=sorted(KINDS), default="stable")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    with open(args.release_json, encoding="utf-8") as source:
        release = json.load(source)
    bodies = payloads(release, args.kind)
    if args.dry_run:
        print(json.dumps(bodies, indent=2, ensure_ascii=False))
        return
    url = os.environ.get("DISCORD_WEBHOOK_URL", "")
    if not url:
        raise RuntimeError("Set DISCORD_WEBHOOK_URL from the workflow webhook secret")
    # In order, one at a time: send() waits for each, so the parts arrive as
    # numbered, and it rides out Discord's per-webhook rate limit.
    for body in bodies:
        send(url, body)
    print(f"Posted the {args.kind} release for {release['tag_name']} "
          f"({len(bodies)} message{'s' if len(bodies) != 1 else ''})")


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"::error::{error}", file=sys.stderr)
        sys.exit(1)
