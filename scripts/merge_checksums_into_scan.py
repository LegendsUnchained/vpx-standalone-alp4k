"""
Final merge step: take the reviewed proposed-checksum-crossref.csv (every
ambiguous row already resolved, every row content-verified by
verify_checksums.py) and attach vpxChecksum / backglassChecksum columns onto
the matching rows of archive-scan-results.csv.

Rows in the crossref file with an empty MatchedPath (FOLDER_MATCHED_NO_ARCHIVE,
NO_TABLE_YML, NONE, SKIPPED_BY_USER, ...) are skipped - there's nothing local
to attach the checksum to. Rows whose VerifyStatus isn't "VERIFIED" (i.e.
MISMATCH - the matched archive's real content doesn't actually match the
table.yml checksum) are also skipped, since attaching a checksum there would
be actively wrong.

The two checksum columns are always reset to blank before reapplying, so
re-running this script after fixing crossref data cleans up any previously
applied (and since-invalidated) values.
"""
import csv
from collections import defaultdict

CROSSREF_PATH = r"M:\wizard tables\proposed-checksum-crossref.csv"
CSV_PATH = r"M:\wizard tables\archive-scan-results.csv"


def main():
    with open(CROSSREF_PATH, "r", encoding="utf-8", newline="") as f:
        crossref_rows = list(csv.DictReader(f))

    # path -> {"vpx": set(checksum), "backglass": set(checksum)}
    checksums_by_path = defaultdict(lambda: {"vpx": set(), "backglass": set()})

    skipped = 0
    skipped_mismatch = 0
    applied = 0
    for r in crossref_rows:
        matched_path = r["MatchedPath"].strip()
        if not matched_path:
            skipped += 1
            continue
        if r.get("VerifyStatus", "VERIFIED") != "VERIFIED":
            skipped_mismatch += 1
            continue
        checksums = [c.strip() for c in r["Checksum"].split(";") if c.strip()]
        # NOTE: multi-path rows use " ||| " as delimiter, not ";" or "; " -
        # some archive filenames contain HTML entities like "&#39;" or
        # "&#33;" which can produce a literal "; " substring, so any
        # semicolon-based delimiter risks chopping a single path in half.
        paths = [p.strip() for p in matched_path.split(" ||| ") if p.strip()]
        for p in paths:
            checksums_by_path[p][r["ChecksumType"]].update(checksums)
        applied += 1

    with open(CSV_PATH, "r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = list(reader.fieldnames)

    if "vpxChecksum" not in fieldnames:
        fieldnames.append("vpxChecksum")
    if "backglassChecksum" not in fieldnames:
        fieldnames.append("backglassChecksum")

    matched_row_count = 0
    for row in rows:
        row["vpxChecksum"] = ""
        row["backglassChecksum"] = ""
        entry = checksums_by_path.get(row["Path"])
        if entry:
            if entry["vpx"]:
                row["vpxChecksum"] = "; ".join(sorted(entry["vpx"]))
            if entry["backglass"]:
                row["backglassChecksum"] = "; ".join(sorted(entry["backglass"]))
            if entry["vpx"] or entry["backglass"]:
                matched_row_count += 1

    with open(CSV_PATH, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Crossref rows applied: {applied}, skipped (no local match): {skipped}, skipped (mismatch): {skipped_mismatch}")
    print(f"archive-scan-results.csv rows updated with a checksum: {matched_row_count}")


if __name__ == "__main__":
    main()
