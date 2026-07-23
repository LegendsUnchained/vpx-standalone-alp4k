#!/usr/bin/env python3
"""Regenerate the ``## Wizard Tables`` and ``## Manual Install Tables`` sections
of the top-level ``README.md`` from the contents of ``external/vpx-*/``.

Data sources per row:

- Wizard vs Manual classification: a table folder is *Wizard* when
  ``external/<folder>/table.yml`` exists and is not explicitly ``enabled: false``
  (matches ``vpsdb.is_disabled`` used elsewhere in this repo).
- Display name: the first ``# <heading>`` in the folder's ``README.md``. Falls
  back to the folder name when the README is missing/empty.
- Backglass / DMD / ROM Required (and Has Puppack, when present): parsed from
  the folder README's Status / Table Statistics table. Both column layouts and
  both check-mark encodings (``:white_check_mark:`` / ``:x:`` and ✅ / ❌) are
  supported.
- FPS: from ``fps:`` in ``table.yml`` for wizard tables; otherwise from the
  folder README's Status table.
- Has Puppack:
    * Wizard rows: True when ``table.yml`` has any of ``pupVPSId``,
      ``pupFileUrl``, ``pupBundled`` (truthy), or ``pupRequired`` (truthy).
    * Manual rows: from the README Status table when the "Has Puppack" column
      is present; otherwise True when the folder contains
      ``use_these_pup_files.zip``, else False.

Sort order is plain case-insensitive lexicographic on display name — the
folder READMEs already carry the ``vpsdb.process_title`` normalization
("Shadow, The", "Addams Family, JP's", etc.), so this matches the existing
main README ordering.

Exit code is always 0. On a successful run a short summary and, if any
folders are missing a README or an unparseable Status table, an "Issues"
section are printed to stdout.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[3]
EXTERNAL_DIR = REPO_ROOT / "external"
README_PATH = REPO_ROOT / "README.md"

WIZARD_HEADER = "## Wizard Tables"
MANUAL_HEADER = "## Manual Install Tables"

CHECK = ":white_check_mark:"
CROSS = ":x:"

TABLE_HEADER = (
    "| Table | Backglass | DMD | ROM Required | Has Puppack | FPS |\n"
    "|:------|:---------:|:---:|:------------:|:---:|:---:|\n"
)

# Any of these cells count as a "checked" value in the per-folder Status table.
_TRUTHY_CELL = {":white_check_mark:", "✅", "✔", "✔️", "yes", "true"}
_FALSY_CELL = {":x:", "❌", "✖", "✖️", "no", "false", "n/a", "-", ""}


def _norm_cell(cell: str) -> str:
    return cell.strip().lower()


def _cell_to_bool(cell: str) -> bool | None:
    """Return True / False for a Status-table cell, or None if unrecognized."""
    n = _norm_cell(cell)
    if n in _TRUTHY_CELL:
        return True
    if n in _FALSY_CELL:
        return False
    return None


def _fmt_bool(value: bool | None) -> str:
    return CHECK if value else CROSS


def _fmt_fps(value) -> str:
    if value is None or value == "":
        return "N/A"
    return str(value)


def load_table_yml(folder: Path) -> dict | None:
    """Return the parsed table.yml as a dict, or None when absent / empty."""
    yml_path = folder / "table.yml"
    if not yml_path.is_file():
        return None
    try:
        with yml_path.open("r", encoding="utf-8") as f:
            data = yaml.safe_load(f)
    except yaml.YAMLError:
        return None
    return data if isinstance(data, dict) else None


def is_wizard(table_yml: dict | None) -> bool:
    """Wizard-eligible iff a table.yml exists and is not enabled: false.

    Matches ``vpsdb.is_disabled``: only an explicit ``False`` disables — a
    missing key or ``None`` means enabled.
    """
    if table_yml is None:
        return False
    return table_yml.get("enabled") is not False


def read_display_name(folder: Path) -> tuple[str, bool, bool]:
    """Return ``(display_name, readme_found, heading_found)``.

    Falls back to the folder name when the README is missing or has no
    ``# `` heading. Whitespace inside the heading is collapsed to a single
    space so the emitted table rows stay on one line.
    """
    readme = folder / "README.md"
    if not readme.is_file():
        return folder.name, False, False
    try:
        # utf-8-sig strips a BOM if one is present — several of the per-folder
        # READMEs are saved with a UTF-8 BOM which would otherwise make the
        # very first line fail the "# " prefix check.
        text = readme.read_text(encoding="utf-8-sig")
    except OSError:
        return folder.name, False, False
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("# ") and not stripped.startswith("## "):
            name = stripped[2:].strip()
            name = re.sub(r"\s+", " ", name)
            if name:
                return name, True, True
            break
    return folder.name, True, False


# Match a markdown table header row containing at least Backglass, DMD and
# FPS in that order. ROM Required and Has Puppack are optional columns and
# may or may not be present in older / variant per-folder READMEs. The
# trailing pipe is optional to tolerate READMEs that forgot to close the
# header row.
_STATUS_HEADER_RE = re.compile(
    r"^\|.*backglass.*\|.*dmd.*\|.*fps.*(?:\|.*)?$",
    re.IGNORECASE,
)
_SEPARATOR_RE = re.compile(r"^\|[\s\-\:\|]+\|?\s*$")


def _split_row(line: str) -> list[str]:
    parts = line.strip().split("|")
    # A well-formed row starts and ends with '|' → empty first/last cells.
    if parts and parts[0] == "":
        parts = parts[1:]
    if parts and parts[-1] == "":
        parts = parts[:-1]
    return [p.strip() for p in parts]


def parse_status_table(folder: Path) -> dict | None:
    """Return {backglass, dmd, rom, puppack, fps} parsed from the folder README.

    ``puppack`` is ``None`` when the "Has Puppack" column isn't present. Any
    unrecognized cell also becomes ``None``. Returns ``None`` when no matching
    table is found in the README.
    """
    readme = folder / "README.md"
    if not readme.is_file():
        return None
    try:
        lines = readme.read_text(encoding="utf-8-sig").splitlines()
    except OSError:
        return None

    for i, line in enumerate(lines):
        if not _STATUS_HEADER_RE.match(line):
            continue
        if i + 2 >= len(lines):
            return None
        sep = lines[i + 1]
        if not _SEPARATOR_RE.match(sep):
            continue
        headers = [h.lower() for h in _split_row(line)]
        cells = _split_row(lines[i + 2])
        # The data row must have the same number of cells as the header for
        # column lookups to be trustworthy. A mismatch (usually a missing
        # ROM cell in an older README) is treated as an unparseable Status
        # table and reported in the run summary.
        if len(cells) != len(headers):
            return None

        def _find(*names):
            for want in names:
                for idx, h in enumerate(headers):
                    if want in h:
                        return idx
            return None

        idx_bg = _find("backglass")
        idx_dmd = _find("dmd")
        idx_rom = _find("rom")
        idx_pup = _find("puppack", "pup pack", "pup")
        idx_fps = _find("fps")

        def _bool_at(idx):
            if idx is None or idx >= len(cells):
                return None
            return _cell_to_bool(cells[idx])

        fps_val = None
        if idx_fps is not None and idx_fps < len(cells):
            raw = cells[idx_fps].strip()
            # Cells sometimes carry trailing notes, e.g. "47 (without music)".
            m = re.search(r"\d+", raw)
            if m:
                fps_val = int(m.group(0))
            elif raw and _norm_cell(raw) not in _FALSY_CELL:
                fps_val = raw

        return {
            "backglass": _bool_at(idx_bg),
            "dmd": _bool_at(idx_dmd),
            "rom": _bool_at(idx_rom),
            "puppack": _bool_at(idx_pup) if idx_pup is not None else None,
            "fps": fps_val,
        }
    return None


def wizard_has_puppack(table_yml: dict) -> bool:
    for key in ("pupVPSId", "pupFileUrl"):
        if table_yml.get(key):
            return True
    for key in ("pupBundled", "pupRequired"):
        if bool(table_yml.get(key)):
            return True
    return False


def manual_has_puppack_hint(folder: Path) -> bool:
    return (folder / "use_these_pup_files.zip").is_file()


def build_row(folder: Path) -> dict:
    """Compute one row for the top-level README from a table folder."""
    table_yml = load_table_yml(folder)
    wizard = is_wizard(table_yml)
    display, readme_found, heading_found = read_display_name(folder)
    status = parse_status_table(folder)

    row: dict = {
        "folder": folder.name,
        "display": display,
        "wizard": wizard,
        "readme_found": readme_found,
        "heading_found": heading_found,
        "status_found": status is not None,
    }

    if status is None:
        status = {"backglass": None, "dmd": None, "rom": None, "puppack": None, "fps": None}

    row["backglass"] = status["backglass"]
    row["dmd"] = status["dmd"]
    row["rom"] = status["rom"]

    if wizard:
        row["puppack"] = wizard_has_puppack(table_yml or {})
        fps = (table_yml or {}).get("fps")
        row["fps"] = fps if fps is not None else status["fps"]
    else:
        if status["puppack"] is not None:
            row["puppack"] = status["puppack"]
        else:
            row["puppack"] = manual_has_puppack_hint(folder)
        row["fps"] = status["fps"]

    return row


def format_row(row: dict) -> str:
    return (
        f"| [{row['display']}](external/{row['folder']}) "
        f"| {_fmt_bool(row['backglass'])} "
        f"| {_fmt_bool(row['dmd'])} "
        f"| {_fmt_bool(row['rom'])} "
        f"| {_fmt_bool(row['puppack'])} "
        f"| {_fmt_fps(row['fps'])} |"
    )


def build_section(rows: list[dict]) -> str:
    body_lines = [format_row(r) for r in rows]
    return TABLE_HEADER + "\n".join(body_lines) + "\n"


def replace_section(text: str, header: str, section_body: str, *, terminate_at_next_header: bool) -> str:
    """Replace the block from ``header`` (a line like ``## Foo``) up to (but not
    including) the next ``## `` heading — or to EOF when
    ``terminate_at_next_header`` is False.

    The header line itself is preserved verbatim (including any trailing
    whitespace on the current line). One blank line + one ``<br>`` block +
    one blank line are placed between the header and the table when the
    section is followed by another ``## `` heading; the section terminates
    with the table when it runs to EOF, matching the current README's shape.
    """
    lines = text.splitlines(keepends=True)
    start = None
    for i, line in enumerate(lines):
        if line.rstrip() == header.rstrip() or line.rstrip().startswith(header):
            # Accept "## Wizard Tables" and "## Wizard Tables " (trailing space).
            if line.strip() == header.strip():
                start = i
                break
    if start is None:
        raise ValueError(f"Section header not found in README: {header!r}")

    end = len(lines)
    if terminate_at_next_header:
        for j in range(start + 1, len(lines)):
            if lines[j].startswith("## "):
                end = j
                break

    # Preserve the existing header line exactly (spacing and all).
    header_line = lines[start]

    # Body: blank line, table, blank line, "<br>", blank line before next ##
    # (or just table for the final section that runs to EOF).
    if terminate_at_next_header:
        replacement = (
            header_line
            + "\n"
            + section_body
            + "\n<br>\n\n"
        )
    else:
        # Preserve the file's final newline convention: end with a single \n.
        replacement = header_line + "\n" + section_body

    return "".join(lines[:start]) + replacement + "".join(lines[end:])


def discover_folders() -> list[Path]:
    if not EXTERNAL_DIR.is_dir():
        return []
    return sorted(
        (p for p in EXTERNAL_DIR.iterdir() if p.is_dir() and p.name.startswith("vpx-")),
        key=lambda p: p.name.lower(),
    )


def sort_rows(rows: list[dict]) -> list[dict]:
    return sorted(rows, key=lambda r: r["display"].casefold())


def render_readme(current_text: str) -> tuple[str, list[dict], list[dict]]:
    folders = discover_folders()
    all_rows = [build_row(f) for f in folders]

    wizard_rows = sort_rows([r for r in all_rows if r["wizard"]])
    manual_rows = sort_rows([r for r in all_rows if not r["wizard"]])

    new_text = replace_section(
        current_text,
        WIZARD_HEADER,
        build_section(wizard_rows),
        terminate_at_next_header=True,
    )
    new_text = replace_section(
        new_text,
        MANUAL_HEADER,
        build_section(manual_rows),
        terminate_at_next_header=False,
    )
    return new_text, wizard_rows, manual_rows


def print_issues(all_rows: list[dict]) -> None:
    missing_readme = [r["folder"] for r in all_rows if not r["readme_found"]]
    missing_heading = [
        r["folder"]
        for r in all_rows
        if r["readme_found"] and not r["heading_found"]
    ]
    missing_status = [
        r["folder"]
        for r in all_rows
        if r["readme_found"] and not r["status_found"]
    ]
    if not (missing_readme or missing_heading or missing_status):
        return
    print("")
    print("Issues:")
    if missing_readme:
        print(f"  Missing README ({len(missing_readme)}):")
        for name in missing_readme:
            print(f"    - external/{name}")
    if missing_heading:
        print(f"  README present but no '# Title' heading ({len(missing_heading)}):")
        for name in missing_heading:
            print(f"    - external/{name}")
    if missing_status:
        print(f"  README present but no parseable Status/Statistics table ({len(missing_status)}):")
        for name in missing_status:
            print(f"    - external/{name}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Do not write README.md; print whether it would change and the diff.",
    )
    args = parser.parse_args(argv)

    if not README_PATH.is_file():
        print(f"ERROR: {README_PATH} not found", file=sys.stderr)
        return 2

    current = README_PATH.read_text(encoding="utf-8")
    new_text, wizard_rows, manual_rows = render_readme(current)

    changed = new_text != current
    all_rows = wizard_rows + manual_rows

    if args.check:
        if changed:
            print("README.md would change.")
            import difflib

            diff = difflib.unified_diff(
                current.splitlines(keepends=True),
                new_text.splitlines(keepends=True),
                fromfile="README.md (current)",
                tofile="README.md (regenerated)",
                n=3,
            )
            sys.stdout.writelines(diff)
        else:
            print("README.md is up to date.")
    else:
        if changed:
            README_PATH.write_text(new_text, encoding="utf-8")
            print(f"Updated {README_PATH}.")
        else:
            print(f"{README_PATH} is up to date.")

    print(
        f"Summary: {len(wizard_rows)} wizard rows, {len(manual_rows)} manual rows."
    )
    print_issues(all_rows)
    return 0


if __name__ == "__main__":
    sys.exit(main())
