"""
Resolve AMBIGUOUS_* rows in proposed-checksum-crossref.csv by extracting the
matching .vpx / .directb2s entry from each candidate archive and comparing its
MD5 against the vpxChecksum / backglassChecksum value(s) from table.yml.

Only rows whose Confidence starts with one of AMBIGUOUS_PREFIXES are touched;
everything else in proposed-checksum-crossref.csv is left as-is.
"""
import csv
import hashlib
import os
import re
import subprocess
import tempfile

SEVENZIP = r"C:\Program Files\7-Zip\7z.exe"
CROSSREF_PATH = r"M:\wizard tables\proposed-checksum-crossref.csv"

AMBIGUOUS_PREFIXES = (
    "AMBIGUOUS_TITLE_MULTI_FILE",
    "AMBIGUOUS_FUZZY_TITLE",
    "AMBIGUOUS_FILENAME",
    "AMBIGUOUS_TITLE_MULTI_FOLDER",
)


def list_entries(archive_path):
    proc = subprocess.run(
        [SEVENZIP, "l", "-slt", "--", archive_path],
        capture_output=True, text=True, encoding="utf-8", errors="replace"
    )
    entries = []
    for line in proc.stdout.splitlines():
        m = re.match(r'^Path = (.+)$', line)
        if m:
            entries.append(m.group(1))
    if len(entries) > 1:
        entries = entries[1:]  # first "Path =" is the archive itself
    return entries


def extract_entry_md5(archive_path, entry_name, tmpdir):
    subprocess.run(
        [SEVENZIP, "e", "-y", f"-o{tmpdir}", "--", archive_path, entry_name],
        capture_output=True, text=True
    )
    base = os.path.basename(entry_name.replace("\\", "/"))
    extracted_path = os.path.join(tmpdir, base)
    if not os.path.exists(extracted_path):
        return None
    h = hashlib.md5()
    with open(extracted_path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    os.remove(extracted_path)
    return h.hexdigest().upper()


def main():
    with open(CROSSREF_PATH, "r", encoding="utf-8", newline="") as f:
        rows = list(csv.DictReader(f))
        fieldnames = list(rows[0].keys())

    resolved, unresolved, multi = 0, 0, 0

    for row in rows:
        if not any(row["Confidence"].startswith(p) for p in AMBIGUOUS_PREFIXES):
            continue

        checksums = [c.strip().upper() for c in row["Checksum"].split(";") if c.strip()]
        candidates = [c.strip() for c in row["Candidates"].split(";") if c.strip()]
        ext = ".vpx" if row["ChecksumType"] == "vpx" else ".directb2s"

        print(f"Resolving {row['VpxCode']} ({row['ChecksumType']}): {len(candidates)} candidate archive(s)...")

        matches = []  # (archive_path, entry_name, md5)
        with tempfile.TemporaryDirectory() as tmpdir:
            for archive in candidates:
                entries = list_entries(archive)
                target_entries = [e for e in entries if e.lower().endswith(ext)]
                for entry in target_entries:
                    md5 = extract_entry_md5(archive, entry, tmpdir)
                    if md5 and md5 in checksums:
                        matches.append((archive, entry, md5))

        unique_archives = sorted(set(m[0] for m in matches))

        if len(unique_archives) == 1:
            row["MatchedPath"] = unique_archives[0]
            row["Confidence"] = "VERIFIED_MD5"
            row["Candidates"] = ""
            resolved += 1
        elif len(unique_archives) > 1:
            row["Confidence"] = "MD5_MULTI_MATCH"
            row["Candidates"] = "; ".join(unique_archives)
            multi += 1
        else:
            row["Confidence"] = "MD5_NO_MATCH"
            unresolved += 1

    with open(CROSSREF_PATH, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"\nResolved via MD5: {resolved}")
    print(f"Still multiple MD5 matches: {multi}")
    print(f"No MD5 match found: {unresolved}")


if __name__ == "__main__":
    main()
