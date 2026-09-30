"""Post a release's new and updated tables to Discord as one embed (stdlib only).

    notify-discord-release.py [--kind stable|testing] [--dry-run] RELEASE_JSON

RELEASE_JSON is the release object from the GitHub API. The webhook comes from
DISCORD_WEBHOOK_URL; --dry-run prints the payload instead of sending it.

stable is the promotion to a published release (promote-release.yml); testing
is a new prerelease candidate (create-testing-release.yml). They differ only in
color and footer. A greeting leads the description: "Happy Wizard Wednesday!"
when the release went out on a Wednesday, US Eastern time, and a wistful
version naming the actual day otherwise.
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

KINDS = {
    "stable": {"color": 0x6BD3F1, "footer": "Stable release"},
    "testing": {"color": 0xF5A623, "footer": "Testing release · beta testers"},
}

# "Wizard Wednesday" is Wednesday on US Eastern time, EDT or EST as the date has it.
EASTERN = ZoneInfo("America/New_York")
DAYS = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
INTRO = "The Wizard team is proud to present the following tables for your enjoyment:"

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
    title = f"Wizard Table Release - {tag if tag.startswith('v') else 'v' + tag}"[:TITLE_MAX]
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


def payload(release, kind):
    return {"embeds": [embed(release, kind)], "allowed_mentions": {"parse": []}}


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
    body = payload(release, args.kind)
    if args.dry_run:
        print(json.dumps(body, indent=2, ensure_ascii=False))
        return
    url = os.environ.get("DISCORD_WEBHOOK_URL", "")
    if not url:
        raise RuntimeError("Set DISCORD_WEBHOOK_URL from the workflow webhook secret")
    send(url, body)
    print(f"Posted the {args.kind} release embed for {release['tag_name']}")


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"::error::{error}", file=sys.stderr)
        sys.exit(1)
