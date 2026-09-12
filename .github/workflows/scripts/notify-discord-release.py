"""Send exactly two Discord posts for a promoted release (stdlib only)."""

import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request


def sections(body):
    notes = {"New tables": [], "Updated tables": []}
    current = None
    for line in body.splitlines():
        if line.startswith("## "):
            heading = line[3:].strip().rstrip(":").lower()
            current = {"newly added tables": "New tables",
                       "updated tables": "Updated tables"}.get(heading)
        elif current:
            notes[current].append(line)
    return {title: "\n".join(lines).strip() for title, lines in notes.items()}


def message(title, notes, release):
    heading = f"## {title} — {release['tag_name']}\n{release['html_url']}\n\n"
    content = heading + (notes or "None in this release.")
    # Each generated table entry occupies one line. Remove whole entries and
    # recalculate the footer so its count and link also fit the message limit.
    def length(text):
        return len(text.encode("utf-16-le")) // 2

    if length(content) > 2000:
        tables = [line for line in notes.splitlines() if line.strip()]
        total = len(tables)
        for kept in range(total - 1, -1, -1):
            footer = (
                f"{total - kept} Additional. View the entire list at "
                "[https://vpxtablemanager.com/catalog](https://vpxtablemanager.com/catalog)"
            )
            content = heading + "\n".join(tables[:kept])
            content += ("\n\n" if kept else "") + footer
            if length(content) <= 2000:
                break
        else:
            raise ValueError("Release heading and footer exceed Discord's message limit")

    payload = {"content": content, "allowed_mentions": {"parse": []}}
    return json.dumps(payload).encode(), "application/json"


def send(url, data, content_type):
    request = urllib.request.Request(url, data=data, headers={
        "Content-Type": content_type, "User-Agent": "ReleaseNotifier/1.0"})
    for attempt in range(5):
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                response.read()
            return
        except urllib.error.HTTPError as error:
            if error.code == 429 and attempt < 4:
                time.sleep(float(json.loads(error.read())["retry_after"]))
                continue
            # Never print the exception URL: it contains the webhook credential.
            raise RuntimeError(f"Discord returned HTTP {error.code}") from None
        except urllib.error.URLError:
            raise RuntimeError("Could not connect to Discord") from None


def main():
    url = os.environ.get("DISCORD_WEBHOOK_URL", "")
    if not url:
        raise RuntimeError("Set the DISCORD_RELEASE_WEBHOOK_URL repository secret")
    parts = urllib.parse.urlsplit(url)
    query = dict(urllib.parse.parse_qsl(parts.query))
    query["wait"] = "true"
    url = urllib.parse.urlunsplit(parts._replace(query=urllib.parse.urlencode(query)))
    with open(sys.argv[1], encoding="utf-8") as source:
        release = json.load(source)
    for title, notes in sections(release.get("body") or "").items():
        send(url, *message(title, notes, release))
        print(f"Posted {title}")


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"::error::{error}", file=sys.stderr)
        sys.exit(1)
