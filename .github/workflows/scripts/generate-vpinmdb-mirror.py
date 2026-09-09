#!/usr/bin/env python3
"""Mirror vpinmediadb's backglass/playfield/FSS art for wizard tables only,
re-encoded as WebP, into a small vpinmediadb-COMPATIBLE json (same shape:
{vpsId: {"1k": {"bg": ..., "table": ..., "fss": ...}}}, just far fewer entries
and WebP URLs instead of vpinmediadb's PNGs) — a drop-in for any consumer that
already speaks vpinmediadb's index format.

Prefers each table's 1k image; falls back to 4k (resized down, same as
everything else) when 1k is missing rather than dropping that image — e.g.
~5 wizard tables only have a 4k.fss. Always published under "1k" in our own
output regardless of which tier it actually came from, since every consumer
only ever reads media[id]["1k"][kind] and we resize to one fixed size anyway.

Both tm-installer's Wizard Catalog page and Table Manager's own wizard UI need
this art, and both currently hot-link vpinmediadb's raw PNGs directly: the
full upstream index covers ~1156 tables (way more than the ~300 in the
wizard), and each PNG is ~1080p / ~3 MB. Measured: WebP q90 at 960x540 (the
largest size anything in either UI actually renders this at) comes out to
~140 KB average, a ~95% reduction — see the "vpinmdb 1k benchmark" in the
implementation notes for the full comparison table.

Usage:
    python generate-vpinmdb-mirror.py --manifest manifest.json --out /tmp/media
"""
import argparse
import json
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from io import BytesIO
from pathlib import Path

import requests
from PIL import Image

VPINMDB_INDEX_URL = "https://raw.githubusercontent.com/superhac/vpinmediadb/main/vpinmdb.json"
DEFAULT_BASE_URL = "https://raw.githubusercontent.com/LegendsUnchained/vpx-standalone-alp4k/manifest/media/"
KINDS = ("bg", "table", "fss")
SIZE = (960, 540)  # largest CSS size anything in tm-installer or tablemanager-fe renders this at
QUALITY = 90
TIMEOUT = 20


def fetch_index():
    resp = requests.get(VPINMDB_INDEX_URL, timeout=30)
    resp.raise_for_status()
    return resp.json()


def pick_source(entry, kind):
    """Prefer 1k; fall back to 4k rather than skipping a table's art entirely.
    Whichever tier is used, we resize down to SIZE anyway, so the source
    resolution doesn't matter downstream. (bg has no 4k variant in
    vpinmediadb — checked across all 1156 entries — so this only ever
    recovers table/fss; measured 5 wizard tables gain an fss this way.)"""
    for tier in ("1k", "4k"):
        url = (entry.get(tier) or {}).get(kind)
        if url:
            return url, tier
    return None, None


def convert_one(vps_id, kind, url, media_dir):
    try:
        resp = requests.get(url, timeout=TIMEOUT)
        resp.raise_for_status()
        with Image.open(BytesIO(resp.content)) as im:
            im = im.convert("RGB")
            im.thumbnail(SIZE, Image.LANCZOS)
            dest = media_dir / vps_id / f"{kind}.webp"
            dest.parent.mkdir(parents=True, exist_ok=True)
            im.save(dest, "WEBP", quality=QUALITY, method=6)
        return vps_id, kind, True
    except Exception as e:
        print(f"[WARN] {vps_id}/{kind}: {e}", file=sys.stderr)
        return vps_id, kind, False


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--manifest", required=True, help="wizard manifest.json, to find which vpsdbId values matter")
    parser.add_argument("--out", required=True, help="output dir: <out>/vpinmdb.json + <out>/media/<id>/<kind>.webp")
    parser.add_argument("--base-url", default=DEFAULT_BASE_URL, help="public URL prefix media/ is published under (default: %(default)s)")
    parser.add_argument("--workers", type=int, default=8)
    args = parser.parse_args()

    manifest = json.loads(Path(args.manifest).read_text())
    wizard_ids = sorted({t.get("vpsdbId") for t in manifest.values() if t.get("vpsdbId")})
    print(f"{len(wizard_ids)} wizard tables carry a vpsdbId")

    index = fetch_index()
    jobs = []
    fallbacks = 0
    for vps_id in wizard_ids:
        entry = index.get(vps_id) or {}
        for kind in KINDS:
            url, tier = pick_source(entry, kind)
            if url:
                jobs.append((vps_id, kind, url))
                if tier == "4k":
                    fallbacks += 1
                    print(f"[INFO] {vps_id}/{kind}: no 1k, using 4k instead")
    print(f"{len(jobs)} images to mirror (up to {len(KINDS)} per table, {fallbacks} via 4k fallback)")

    out_dir = Path(args.out)
    media_dir = out_dir / "media"
    base_url = args.base_url if args.base_url.endswith("/") else args.base_url + "/"

    trimmed = {}
    ok = 0
    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = [pool.submit(convert_one, vid, kind, url, media_dir) for vid, kind, url in jobs]
        for fut in as_completed(futures):
            vps_id, kind, success = fut.result()
            if success:
                trimmed.setdefault(vps_id, {}).setdefault("1k", {})[kind] = f"{base_url}{vps_id}/{kind}.webp"
                ok += 1

    out_dir.mkdir(parents=True, exist_ok=True)
    with open(out_dir / "vpinmdb.json", "w") as f:
        json.dump(trimmed, f, indent=2, sort_keys=True)

    print(f"Mirrored {ok}/{len(jobs)} images for {len(trimmed)} tables -> {out_dir / 'vpinmdb.json'}")
    if jobs and ok == 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
