#!/usr/bin/env python3
"""Render the wizard manifest for one or more tables LOCALLY.

Uses the exact same VPSDB resolution the release uses (vpsdb.get_table_meta), so
the output matches what a release would produce for the resolved metadata — but
WITHOUT touching GitHub: no release, no asset upload, no git history required.
The two release-only fields the manifest gains at publish time (`repoConfig`, the
asset download URL, and `configVersion`, the table folder's commit hash) are
intentionally absent here — everything else is byte-for-byte what ships.

Use it to preview / review the JSON a table.yml renders to before opening or
merging a PR.

Examples:
    # one table (folder name), pretty JSON to stdout
    python generate-manifest.py vpx-starwars

    # a few tables by path, written to a file
    python generate-manifest.py external/vpx-starwars/table.yml -o manifest.json

    # every table under external/ (same discovery the release uses)
    python generate-manifest.py

Progress/resolution logs go to stderr; only the JSON manifest goes to stdout, so
`python generate-manifest.py vpx-starwars > out.json` yields clean JSON.
"""
import argparse
import contextlib
import json
import sys
from pathlib import Path

import vpsdb

EXTERNAL_DIR = Path("external")


def resolve_files(tokens):
    """Map CLI tokens to table.yml paths.

    Each token may be a table.yml path, a table folder (path or bare name like
    "vpx-starwars", resolved under external/). With no tokens, discover every
    external/vpx-*/table.yml — the same set the release builds.
    """
    if not tokens:
        return sorted(str(p) for p in EXTERNAL_DIR.glob("vpx-*/table.yml"))

    files = []
    missing = []
    for token in tokens:
        candidate = Path(token)
        if candidate.is_file():
            files.append(str(candidate))
        elif candidate.is_dir() and (candidate / "table.yml").is_file():
            files.append(str(candidate / "table.yml"))
        elif (EXTERNAL_DIR / token / "table.yml").is_file():
            files.append(str(EXTERNAL_DIR / token / "table.yml"))
        else:
            missing.append(token)

    for token in missing:
        print(f"WARNING: no table.yml found for '{token}'", file=sys.stderr)
    return files


def render(files, strict):
    """Render the manifest dict for the given table.yml files.

    strict=False (default) mirrors a lenient preview: a table that fails to
    resolve (e.g. an unknown VPS id) is warned about and skipped so the rest
    still render. strict=True mirrors the release/validator rendering, which
    exits non-zero on the first unresolved id (warn_on_error=False).

    vpsdb.get_table_meta logs progress via print(); redirect that to stderr so
    stdout stays clean JSON.
    """
    with contextlib.redirect_stdout(sys.stderr):
        tables = vpsdb.get_table_meta(files, warn_on_error=not strict)

    # Apply the same display-name transform the release applies at publish time
    # (vpsdb.process_title), so the previewed `name` matches what ships.
    for data in tables.values():
        name = data.get("name")
        if isinstance(name, str) and name:
            data["name"] = vpsdb.process_title(
                name, data.get("manufacturer", ""), data.get("year", "")
            )
    return tables


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Render the wizard manifest for tables locally (no GitHub/release).",
    )
    parser.add_argument(
        "tables",
        nargs="*",
        help="table folder names (vpx-starwars), folders, or table.yml paths. "
        "Default: every table under external/.",
    )
    parser.add_argument(
        "-o",
        "--output",
        help="write the manifest JSON here (default: stdout).",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="exit non-zero if any table fails to resolve (matches the release/validator).",
    )
    args = parser.parse_args(argv)

    files = resolve_files(args.tables)
    if not files:
        print("No table.yml files to render.", file=sys.stderr)
        return 1

    tables = render(files, args.strict)

    payload = json.dumps(tables, indent=2)
    if args.output:
        Path(args.output).write_text(payload + "\n")
        print(f"Wrote {args.output} ({len(tables)} table(s))", file=sys.stderr)
    else:
        print(payload)
    return 0


if __name__ == "__main__":
    sys.exit(main())
