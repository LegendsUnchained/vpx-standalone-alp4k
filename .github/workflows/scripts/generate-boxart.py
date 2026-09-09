#!/usr/bin/env python3
"""Render every table's launcher.png box art as full-size WebP, for the CORS
mirror branch only — this never touches the device-installed release zip
(generate-release.py), which still ships the PNG untouched.

tm-installer's public Wizard Catalog page hot-links this for its list view and
detail drawer. Re-encoding at q90 with no resize measured ~88% smaller than
the source PNG (~1.08 MB -> ~145 KB average) with no separate small-thumbnail
variant needed on top: 145 KB is already small enough for a page listing
~300 tables, and the browser caches it across every place it's reused.

Usage:
    python generate-boxart.py --out /tmp/boxart
    python generate-boxart.py --external-dir release-checkout/external --out /tmp/boxart
"""
import argparse
import sys
from pathlib import Path

from PIL import Image

QUALITY = 90


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--external-dir", default="external", help="directory containing <table-key>/launcher.png folders (default: %(default)s)")
    parser.add_argument("--out", required=True, help="output directory for <table-key>.webp files")
    args = parser.parse_args()

    external_dir = Path(args.external_dir)
    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)

    sources = sorted(external_dir.glob("*/launcher.png"))
    if not sources:
        print(f"No launcher.png files found under {external_dir}/", file=sys.stderr)
        sys.exit(1)

    written = 0
    for src in sources:
        key = src.parent.name
        dest = out_dir / f"{key}.webp"
        try:
            with Image.open(src) as im:
                im.convert("RGB").save(dest, "WEBP", quality=QUALITY, method=6)
            written += 1
        except Exception as e:  # noqa: BLE001 - one bad image shouldn't fail the whole release
            print(f"[WARN] could not convert {src}: {e}", file=sys.stderr)

    print(f"Wrote {written}/{len(sources)} box-art images to {out_dir}/")


if __name__ == "__main__":
    main()
