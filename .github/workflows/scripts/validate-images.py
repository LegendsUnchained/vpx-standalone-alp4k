#!/usr/bin/env python3
"""Enforce the repository's image standards.

Checks, in order of how often they have actually gone wrong:

1. A file named ``*.png`` really is a PNG. JPEG and WEBP files have repeatedly
   been committed under a ``.png`` name; nothing rejects them at upload time and
   they then break any tool that trusts the extension.
2. No ``*.png`` is an animated PNG. The launcher draws a single frame, so an
   APNG silently ships ~40 unused frames of payload.
3. Table art is exactly the resolution the launcher renders at.

Pure stdlib on purpose: this runs in CI before any pip install, and as a
pre-commit hook on contributor machines.

Usage:
    validate-images.py <path> [<path> ...]   # check just these files
    validate-images.py --all                 # check every tracked image
"""
import os
import struct
import subprocess
import sys

PNG_SIG = bytes([0x89]) + b"PNG\r\n" + bytes([0x1A]) + b"\n"

# basename -> (width, height). Only art the launcher renders at a fixed size.
RESOLUTIONS = {
    "launcher.png": (640, 960),
    "backglass.png": (1920, 1080),
    "dmd.png": (1920, 1200),
}
TABLES_DIR = "tables"


def annotate(path, message):
    """Emit a GitHub Actions error annotation, and a plain line locally."""
    if os.environ.get("GITHUB_ACTIONS") == "true":
        print(f"::error file={path}::{message}")
    print(f"  {path}: {message}")


def read_png(path):
    """Return (width, height, is_apng) or raise ValueError if not a PNG."""
    with open(path, "rb") as fh:
        data = fh.read()
    if data[:8] != PNG_SIG:
        kind = "JPEG" if data[:3] == b"\xff\xd8\xff" else \
               "WEBP" if data[:4] == b"RIFF" and data[8:12] == b"WEBP" else \
               "GIF" if data[:4] == b"GIF8" else "not an image we recognize"
        raise ValueError(kind)
    if data[12:16] != b"IHDR":
        raise ValueError("PNG header is malformed (no IHDR)")
    width, height = struct.unpack(">II", data[16:24])
    is_apng = False
    offset = 8
    while offset + 8 <= len(data):
        length = struct.unpack(">I", data[offset:offset + 4])[0]
        ctype = data[offset + 4:offset + 8]
        if ctype == b"acTL":
            is_apng = True
        offset += 12 + length
        if ctype == b"IEND":
            break
    return width, height, is_apng


def check(path):
    """Return the number of problems found in one file."""
    if not os.path.isfile(path):
        return 0  # deleted in this PR
    problems = 0
    try:
        width, height, is_apng = read_png(path)
    except ValueError as exc:
        annotate(path, f"named .png but the contents are {exc}. Re-export it as a real PNG.")
        return 1
    except OSError as exc:
        annotate(path, f"could not be read: {exc}")
        return 1

    if is_apng:
        annotate(path, "is an animated PNG (APNG). The launcher only ever draws the "
                       "first frame, so export a single-frame PNG instead.")
        problems += 1

    name = os.path.basename(path)
    parts = path.split(os.sep)
    in_table_folder = len(parts) >= 3 and parts[0] == TABLES_DIR
    if name in RESOLUTIONS and in_table_folder:
        want_w, want_h = RESOLUTIONS[name]
        if (width, height) != (want_w, want_h):
            annotate(path, f"is {width}x{height} but {name} must be exactly "
                           f"{want_w}x{want_h}. Resize it before committing "
                           f"(aspect ratio {want_w}:{want_h}).")
            problems += 1
    return problems


def tracked_images():
    out = subprocess.run(["git", "ls-files", "*.png"],
                         capture_output=True, text=True).stdout
    return [line for line in out.split("\n") if line]


def main(argv):
    if not argv:
        print(__doc__)
        return 2
    paths = tracked_images() if argv == ["--all"] else [p for p in argv if p.endswith(".png")]
    if not paths:
        print("No .png files to check.")
        return 0

    problems = sum(check(p) for p in paths)
    print(f"\nChecked {len(paths)} image(s); {problems} problem(s) found.")
    if problems:
        print("\nRequired resolutions:")
        for name, (w, h) in RESOLUTIONS.items():
            print(f"  {TABLES_DIR}/<table>/{name}: {w}x{h}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
