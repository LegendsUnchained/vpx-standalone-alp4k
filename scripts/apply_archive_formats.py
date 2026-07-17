"""
Updates external/vpx-<code>/table.yml with two pieces of archive metadata,
derived from rows in proposed-checksum-crossref.csv that verify_checksums.py
confirmed actually match (VerifyStatus == "VERIFIED"):

1. `vpxArchiveFormat` / `backglassArchiveFormat`: the compression format
   (zip/rar/7z) of the matched local archive, inserted right before the
   corresponding *Checksum field (matching the existing pupArchiveFormat /
   altSoundArchiveFormat convention).

2. `vpxChecksum` / `backglassChecksum`: converted from a single string into a
   YAML list containing the existing value (the extracted .vpx/.directb2s
   file's MD5) PLUS the archive file's own MD5 (the "MD5" column in
   archive-scan-results.csv, i.e. the checksum of the .zip/.rar/.7z itself).

Only rows with VerifyStatus == "VERIFIED" are used, so we never derive
metadata from a wrong/mismatched archive. Safe to re-run (idempotent).

Usage:
    python apply_archive_formats.py                # process every vpx code
    python apply_archive_formats.py vpx-123         # process just one (test run)
    python apply_archive_formats.py vpx-123 vpx-24  # process a specific list
"""
import csv
import os
import re
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXTERNAL_ROOT = os.path.join(REPO_ROOT, "external")
CROSSREF_PATH = r"M:\wizard tables\proposed-checksum-crossref.csv"
SCAN_PATH = r"M:\wizard tables\archive-scan-results.csv"

FORMAT_FIELD = {"vpx": "vpxArchiveFormat", "backglass": "backglassArchiveFormat"}
CHECKSUM_FIELD = {"vpx": "vpxChecksum", "backglass": "backglassChecksum"}


def load_path_info():
    """Path -> {"format": "zip", "md5": "AABBCC..."}"""
    with open(SCAN_PATH, "r", encoding="utf-8-sig", newline="") as f:
        rows = list(csv.DictReader(f))
    return {
        r["Path"]: {"format": r["Format"].strip().lower(), "md5": r["MD5"].strip().upper()}
        for r in rows
    }


def load_metadata_by_code():
    """vpx_code -> {"vpx": {"format": .., "archive_md5s": [..]}, "backglass": {...}}"""
    path_info = load_path_info()
    with open(CROSSREF_PATH, "r", encoding="utf-8", newline="") as f:
        rows = list(csv.DictReader(f))

    result = {}
    for r in rows:
        if r["VerifyStatus"] != "VERIFIED":
            continue
        matched_path = r["MatchedPath"].strip()
        if not matched_path:
            continue
        paths = [p.strip() for p in matched_path.split(" ||| ") if p.strip()]
        infos = [path_info[p] for p in paths if p in path_info]
        if not infos:
            continue

        formats = {i["format"] for i in infos}
        md5s = {i["md5"] for i in infos}

        if len(formats) > 1:
            print(f"  ! {r['VpxCode']} ({r['ChecksumType']}): multiple differing formats {formats}, skipping")
            continue

        result.setdefault(r["VpxCode"], {})[r["ChecksumType"]] = {
            "format": formats.pop(),
            "archive_md5s": sorted(md5s),  # usually just one; Central Park style rows can have 2
        }
    return result


def update_format_field(lines, field_name, fmt):
    """Insert/update `<field_name>: "<fmt>"` immediately before the matching
    checksum field's block. Returns True if the file was changed."""
    format_line_re = re.compile(rf'^{field_name}:\s*"(.*)"\s*$')

    existing_idx = next((i for i, l in enumerate(lines) if format_line_re.match(l)), None)
    if existing_idx is not None:
        m = format_line_re.match(lines[existing_idx])
        if m.group(1) != fmt:
            lines[existing_idx] = f'{field_name}: "{fmt}"\n'
            return True
        return False

    checksum_field = field_name.replace("ArchiveFormat", "Checksum")
    checksum_line_re = re.compile(rf'^{checksum_field}:\s*')
    checksum_idx = next((i for i, l in enumerate(lines) if checksum_line_re.match(l)), None)
    if checksum_idx is None:
        return False  # nothing to anchor to
    lines.insert(checksum_idx, f'{field_name}: "{fmt}"\n')
    return True


def find_checksum_block(lines, field_name):
    """
    Locate the `<field_name>:` field, whether it's a single quoted-string
    line or an existing YAML list. Returns (start, end, values) where
    lines[start:end] is the whole field block and values is the list of
    checksum strings currently present ([] / single item / multiple items).
    Returns (None, None, None) if the field isn't present.
    """
    single_re = re.compile(rf'^{field_name}:\s*"(.*)"\s*$')
    list_start_re = re.compile(rf'^{field_name}:\s*$')
    item_re = re.compile(r'^\s*-\s*"(.*)"\s*$')

    for i, line in enumerate(lines):
        m = single_re.match(line)
        if m:
            return i, i + 1, [m.group(1)]
        if list_start_re.match(line):
            j = i + 1
            values = []
            while j < len(lines) and item_re.match(lines[j]):
                values.append(item_re.match(lines[j]).group(1))
                j += 1
            return i, j, values
    return None, None, None


def update_checksum_field(lines, field_name, archive_md5s):
    """Ensure every value in archive_md5s is present in the field's YAML
    list, appending any missing ones (converting a single string to a list
    if needed). Returns True if the file was changed."""
    start, end, values = find_checksum_block(lines, field_name)
    if start is None:
        return False

    missing = [m for m in archive_md5s if m not in values]
    if not missing:
        return False

    new_values = values + missing
    new_lines = [f'{field_name}:\n'] + [f'  - "{v}"\n' for v in new_values]
    lines[start:end] = new_lines
    return True


def update_table_yml(vpx_code, metadata):
    """metadata: {"vpx": {"format": "zip", "archive_md5s": [...]}, "backglass": {...}}"""
    yml_path = os.path.join(EXTERNAL_ROOT, vpx_code, "table.yml")
    if not os.path.exists(yml_path):
        print(f"  ! {vpx_code}: no table.yml found, skipping")
        return False

    with open(yml_path, "r", encoding="utf-8-sig") as f:
        lines = f.readlines()

    changed = False
    for checksum_type, info in metadata.items():
        field_name = FORMAT_FIELD[checksum_type]
        checksum_field = CHECKSUM_FIELD[checksum_type]

        if update_format_field(lines, field_name, info["format"]):
            changed = True
        else:
            checksum_line_re = re.compile(rf'^{checksum_field}:\s*')
            if not any(checksum_line_re.match(l) for l in lines):
                print(f"  ! {vpx_code}: no {checksum_field} line found, skipping {field_name}")

        if update_checksum_field(lines, checksum_field, info["archive_md5s"]):
            changed = True

    if changed:
        with open(yml_path, "w", encoding="utf-8", newline="\n") as f:
            f.writelines(lines)
    return changed


def main():
    requested = sys.argv[1:]
    metadata_by_code = load_metadata_by_code()

    codes = requested if requested else sorted(metadata_by_code.keys())

    updated = 0
    for code in codes:
        metadata = metadata_by_code.get(code)
        if not metadata:
            print(f"  ! {code}: no VERIFIED archive metadata available, skipping")
            continue
        if update_table_yml(code, metadata):
            print(f"  + {code}: {metadata}")
            updated += 1
        else:
            print(f"  = {code}: already up to date ({metadata})")

    print(f"\nUpdated {updated}/{len(codes)} table.yml file(s)")


if __name__ == "__main__":
    main()
