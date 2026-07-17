"""
Full verification pass: for every row in proposed-checksum-crossref.csv that
has a MatchedPath (i.e. was resolved with some confidence level, not just the
previously-ambiguous ones), extract the matching .vpx / .directb2s entry from
the archive and compare its real MD5 against the Checksum value(s) pulled from
table.yml.

Adds two columns to the CSV:
  - VerifyStatus: VERIFIED / MISMATCH / NO_FILE_OF_TYPE / EXTRACT_ERROR / SKIPPED
  - VerifyDetail: the actual MD5(s) found, for mismatches
"""
import csv
import hashlib
import os
import subprocess
import tempfile

SEVENZIP = r"C:\Program Files\7-Zip\7z.exe"
CROSSREF_PATH = r"M:\wizard tables\proposed-checksum-crossref.csv"


def hash_file(path):
    h = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest().upper()


def extract_by_extension(archive_path, ext, tmpdir):
    """
    Extract every entry ending in `ext` (e.g. ".vpx") from the archive using a
    wildcard pattern rather than an exact entry name. Exact-name matching is
    unreliable here: 7-Zip's -slt listing round-trips filenames containing
    smart quotes / apostrophes through the console in a way that doesn't
    always match byte-for-byte when fed back in as an extraction filter,
    causing "No files to process" even though the file genuinely exists.
    Wildcard matching sidesteps that entirely.
    Returns a list of MD5 hashes (uppercase) for every extracted file.
    """
    proc = subprocess.run(
        [SEVENZIP, "e", "-y", "-r", f"-o{tmpdir}", "--", archive_path, f"*{ext}"],
        capture_output=True, text=True, encoding="utf-8", errors="replace"
    )
    hashes = []
    for name in os.listdir(tmpdir):
        full = os.path.join(tmpdir, name)
        if os.path.isfile(full) and name.lower().endswith(ext):
            hashes.append(hash_file(full))
            os.remove(full)
    return hashes, proc.returncode


def main():
    with open(CROSSREF_PATH, "r", encoding="utf-8", newline="") as f:
        rows = list(csv.DictReader(f))
        fieldnames = list(rows[0].keys())

    if "VerifyStatus" not in fieldnames:
        fieldnames.append("VerifyStatus")
    if "VerifyDetail" not in fieldnames:
        fieldnames.append("VerifyDetail")

    counts = {}
    total = sum(1 for r in rows if r["MatchedPath"].strip())
    done = 0

    with tempfile.TemporaryDirectory() as tmpdir:
        for row in rows:
            matched_path = row["MatchedPath"].strip()
            if not matched_path:
                row["VerifyStatus"] = "SKIPPED"
                row["VerifyDetail"] = ""
                continue

            done += 1
            expected = [c.strip().upper() for c in row["Checksum"].split(";") if c.strip()]
            ext = ".vpx" if row["ChecksumType"] == "vpx" else ".directb2s"
            # NOTE: multi-path rows use " ||| " as delimiter, not ";" or "; " -
            # some archive filenames contain HTML entities like "&#39;" or
            # "&#33;" which can produce a literal "; " substring, so any
            # semicolon-based delimiter risks chopping a single path in half.
            archives = [p.strip() for p in matched_path.split(" ||| ") if p.strip()]

            found_md5s = set()
            error = False
            no_file = True

            for archive in archives:
                if not os.path.exists(archive):
                    error = True
                    continue
                hashes, rc = [], None
                for attempt in range(3):
                    try:
                        hashes, rc = extract_by_extension(archive, ext, tmpdir)
                    except Exception:
                        error = True
                        break
                    if hashes:
                        break  # got a result, no need to retry
                if hashes:
                    no_file = False
                    found_md5s.update(hashes)
                elif rc is not None and rc != 0:
                    error = True

            if no_file and not found_md5s:
                status = "NO_FILE_OF_TYPE"
            elif error and not found_md5s:
                status = "EXTRACT_ERROR"
            elif found_md5s & set(expected):
                status = "VERIFIED"
            else:
                status = "MISMATCH"

            row["VerifyStatus"] = status
            row["VerifyDetail"] = "; ".join(sorted(found_md5s))
            counts[status] = counts.get(status, 0) + 1

            if done % 25 == 0:
                print(f"...{done}/{total} checked")

    with open(CROSSREF_PATH, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print("\nVerification summary:")
    for k, v in sorted(counts.items()):
        print(f"  {k}: {v}")


if __name__ == "__main__":
    main()
