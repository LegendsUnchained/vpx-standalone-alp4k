import collections
import json
import os
import re
import sys
from pathlib import Path

import requests
import yaml


def pick_urls(urls):
    """Return every non-broken download URL, in vpsdb order.

    vpsdb lists one or more mirrors per file and flags dead ones with
    ``"broken": true``. This returns the usable ones with order preserved, so
    element 0 is still the primary link. An empty list means nothing is usable —
    the caller decides whether that skips the table (an essential component) or
    just drops the field (an optional one). Also fixes the previous unguarded
    ``urls[0]`` access, which raised IndexError on an empty url list.
    """
    return [u["url"] for u in (urls or []) if u.get("url") and not u.get("broken")]


def is_disabled(data):
    """Whether a parsed table.yml is disabled.

    Only an explicit ``enabled: false`` disables a table — ``enabled: null`` or
    an absent key means enabled. Shared by get_table_meta and
    generate-release-notes.py so the "is this table on?" test can't drift
    between them (generate-release-notes previously used truthiness, which wrongly
    treated ``enabled: null`` as disabled).
    """
    return data.get("enabled") is False


def as_url_list(value):
    """Wrap an author-supplied override URL as a one-element list.

    Author overrides (``*UrlOverride`` in table.yml) are single strings; the
    manifest now carries every URL field as an array of mirrors, so a lone
    override is normalized to ``[url]``. None/empty stays None.
    """
    if not value:
        return None
    if isinstance(value, str):
        return [value]
    return list(value)


def normalize_checksums(value):
    """Normalize a checksum field to a list of lowercased strings.

    Checksum fields may hold multiple acceptable MD5 hashes. Accepts a list
    (the current format) or a bare string (legacy single-checksum format),
    and returns a list of lowercased hashes. Returns None when absent or empty
    so downstream required-checksum checks can test for None.
    """
    if value is None:
        return None
    if isinstance(value, str):
        value = [value]
    normalized = [str(item).lower() for item in value if item]
    return normalized or None


def normalize_dict_list(value):
    """Normalize a field that may be a single mapping or a list of mappings
    into a list of mappings. Absent/None becomes an empty list. Used by the
    additionalRoms field, which is authored as a list of ROM objects.
    """
    if value is None:
        return []
    if isinstance(value, dict):
        return [value]
    return list(value)


def process_title(title, manufacturer, year):
    """Transform a title for proper sorting, moving a leading "The" to the end,
    and handling optional "JP's" / "JPs" prefixes, moving them after the comma
    when 'The' is not present. Returns "<name> (<manufacturer> <year>)".

    Shared by generate-release.py (release manifest) and generate-manifest.py
    (local preview) so both format the display name identically.
    """
    name = ""
    match_the = re.match(r"^(JP'?s\s*)?(The)\s+(.+)$", title)
    match_jps = re.match(r"^(JP'?s)\s+(.+)$", title)
    if match_the and match_the.group(2):
        prefix = match_the.group(1) or ""
        name = f"{match_the.group(3)}, {prefix}{match_the.group(2)}"
    elif match_jps:
        name = f"{match_jps.group(2)}, {match_jps.group(1)}"
    else:
        name = title
    return f"{name} ({manufacturer} {year})"


class VPSDB:
    def __init__(
        self, url="https://virtualpinballspreadsheet.github.io/vps-db/db/vpsdb.json"
    ):
        self.url = url
        self.tables = self._fetch_tables()

    def _fetch_tables(self):
        print(f"Fetching VPSDB from {self.url}")
        try:
            response = requests.get(self.url)
            response.raise_for_status()  # Raises HTTPError for bad responses (4xx and 5xx)
            return response.json()  # Parse JSON data
        except requests.exceptions.HTTPError as http_err:
            print(f"HTTP error occurred: {http_err}")
        except requests.exceptions.RequestException as req_err:
            print(f"Request error occurred: {req_err}")
        except json.JSONDecodeError as json_err:
            print(f"JSON decode error: {json_err}")
        return []

    def get_table(self, id):
        return next((table for table in self.tables if table.get("id") == id), None)

    def get_altcolor_by_id(self, id):
        for table in self.tables:
            if "altColorFiles" in table:
                for altColorFile in table["altColorFiles"]:
                    if altColorFile.get("id") == id:
                        return altColorFile
        return None

    def get_altsound_by_id(self, id):
        for table in self.tables:
            if "altSoundFiles" in table:
                for altSoundFile in table["altSoundFiles"]:
                    if altSoundFile.get("id") == id:
                        return altSoundFile
        return None

    def get_backglass_by_id(self, id):
        for table in self.tables:
            if "b2sFiles" in table:
                for b2sFile in table["b2sFiles"]:
                    if b2sFile.get("id") == id:
                        return b2sFile
        return self.get_tablefile_by_id(id)

    def get_diff_by_id(self, id):
        table = self.get_tablefile_by_id(id)
        if table:
            if "features" in table:
                if "VPU Patch" in table["features"]:
                    return table
        return None
            
    def get_pup_by_id(self, id):
        for table in self.tables:
            if "pupPackFiles" in table:
                for pupPackFile in table["pupPackFiles"]:
                    if pupPackFile.get("id") == id:
                        return pupPackFile
        return None
        
    def get_rom_by_id(self, id):
        for table in self.tables:
            if "romFiles" in table:
                for romFile in table["romFiles"]:
                    if romFile.get("id") == id:
                        return romFile
        return None

    def get_tablefile_by_id(self, id):
        for table in self.tables:
            if "tableFiles" in table:
                for tableFile in table["tableFiles"]:
                    if tableFile.get("id") == id:
                        return tableFile
        return None

    def get_tutorialfile_by_id(self, id):
        for table in self.tables:
            if "tutorialFiles" in table:
                for tutorialFile in table["tutorialFiles"]:
                    if tutorialFile.get("id") == id:
                        return tutorialFile
        return None


def write_summary(tables, skipped, warnings):
    """Print a run summary and, in CI, append a markdown table to the job
    summary (GITHUB_STEP_SUMMARY).

    Skips (disabled, broken-link, broken-entry, not-found) and warnings
    (optional components dropped) are expected outcomes, not errors — this makes
    them visible instead of buried in interleaved per-table logs. Always prints
    to stdout so local generate-manifest.py runs benefit too.
    """
    included = len(tables)
    total = included + len(skipped)
    groups = collections.Counter(s["group"] for s in skipped)

    # Mirror stats, taken from the primary table file.
    single = multi = max_mirrors = 0
    for meta in tables.values():
        urls = meta.get("tableFileUrl")
        n = len(urls) if isinstance(urls, list) else 0
        if n > 1:
            multi += 1
            max_mirrors = max(max_mirrors, n)
        elif n == 1:
            single += 1

    print(
        f"\nSummary: {total} tables — {included} included, {len(skipped)} skipped "
        f"({groups.get('broken_link', 0)} broken links, "
        f"{groups.get('broken_entry', 0)} broken entries, "
        f"{groups.get('disabled', 0)} disabled, "
        f"{groups.get('not_found', 0)} not found in VPSDB)"
    )
    print(f"         mirrors: {single} single-url, {multi} multi-url (max {max_mirrors})")

    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        return
    md = [
        "## VPSDB manifest\n\n",
        f"- **{included}** tables included, **{len(skipped)}** skipped\n",
        f"- broken links: {groups.get('broken_link', 0)} · "
        f"broken entries: {groups.get('broken_entry', 0)} · "
        f"disabled: {groups.get('disabled', 0)} · "
        f"not found: {groups.get('not_found', 0)}\n",
        f"- mirrors: {single} single-url, {multi} multi-url (max {max_mirrors})\n",
    ]
    if skipped:
        md.append("\n### Skipped\n\n| Table | Reason |\n|---|---|\n")
        md += [f"| {s['folder']} | {s['message']} |\n" for s in skipped]
    if warnings:
        md.append("\n### Warnings\n\n| Table | Detail |\n|---|---|\n")
        md += [f"| {w['folder']} | {w['message']} |\n" for w in warnings]
    with open(summary_path, "a") as f:
        f.write("".join(md))


def get_table_meta(files, warn_on_error=True):
    error_prefix = "ERROR"
    if warn_on_error:
        error_prefix = "WARNING"

    tables = {}
    # Structured accounting so the run ends with a summary rather than only
    # interleaved per-table prints. Broken links and disabled tables are normal,
    # expected outcomes (not errors) under either warn_on_error mode.
    skipped = []
    warnings = []
    vpsdb = VPSDB()

    def skip(folder, group, message):
        skipped.append({"folder": folder, "group": group, "message": message})
        print(f"SKIP   {folder}  {message}")

    def warn(folder, message):
        warnings.append({"folder": folder, "message": message})
        print(f"WARN   {folder}  {message}")

    # A VPS id declared in table.yml that isn't in VPSDB. Still a hard error in
    # strict mode (validate-table-yaml); a recorded skip otherwise. The caller
    # `continue`s after calling this.
    def not_found(folder, label, vps_id):
        print(f"{error_prefix}: {label} {vps_id} not found in VPSDB")
        if not warn_on_error:
            sys.exit(1)
        skip(folder, "not_found", f"{label} {vps_id} not found in VPSDB")

    # Resolve a declared component's URL list and decide whether an essential
    # component with no usable link skips the table. Returns True to keep the
    # table, False to skip it.
    #
    # Precedence is preserved per the original code: most components let VPSDB
    # win (overwrite any author override), while diff/altSound let the author
    # override win (override_wins=True). In every case, when VPSDB has no usable
    # mirror the author override is used as a fallback, and only when neither is
    # usable is the table skipped.
    def resolve_essential_urls(folder, table_meta, field, urls, vps_id, override_wins=False):
        if override_wins and table_meta.get(field):
            return True  # author override wins; don't touch VPSDB urls
        picked = pick_urls(urls)
        if picked:
            table_meta[field] = picked
        elif not table_meta.get(field):
            skip(folder, "broken_link", f"{field}: all urls broken ({vps_id})")
            return False
        # else: VPSDB all-broken but an author override exists — keep it.
        return True

    for table_yaml in files:
        path = Path(table_yaml)
        folder_name = path.parent.name

        print(f"Processing {folder_name}")
        with open(table_yaml, "r") as table_data:
            data = yaml.safe_load(table_data)

        # Disabled tables are excluded up front, before any VPSDB resolution, so
        # a disabled table carrying a stale/removed VPS id can never fail the run.
        if is_disabled(data):
            skip(folder_name, "disabled", "disabled in table.yml")
            continue

        altSoundVPSId = data.get("altSoundVPSId")
        altSoundUrlOverride = data.get("altSoundUrlOverride")
        backglassVPSId = data.get("backglassVPSId")
        coloredROMVPSId = data.get("coloredROMVPSId")
        diffVPSId = data.get("diffVPSId")
        pupVPSId = data.get("pupVPSId")
        romVPSId = data.get("romVPSId")
        tableVPSId = data.get("tableVPSId")
        tutorialVPSId = data.get("tutorialVPSId")
        vpxVPSId = data.get("vpxVPSId")
        
        # Checksum fields are lists of acceptable MD5 hashes so a table can
        # accept more than one valid file. A single string (legacy format) is
        # tolerated and normalized to a one-element list. Absent/empty stays
        # None so the required-checksum checks in validate-table-yaml.py keep
        # working.
        altSoundChecksum = normalize_checksums(data.get("altSoundChecksum"))
        backglassChecksum = normalize_checksums(data.get("backglassChecksum"))
        coloredROMChecksum = normalize_checksums(data.get("coloredROMChecksum"))
        diffChecksum = normalize_checksums(data.get("diffChecksum"))
        romChecksum = normalize_checksums(data.get("romChecksum"))
        vpxChecksum = normalize_checksums(data.get("vpxChecksum"))
        pupChecksum = normalize_checksums(data.get("pupChecksum"))

        table_meta = {
            "altSoundAuthors": data.get("altSoundAuthorsOverride"),
            "altSoundBundled": data.get("altSoundBundled"),
            "altSoundChecksum": altSoundChecksum,
            "altSoundFileUrl": as_url_list(altSoundUrlOverride),
            "altSoundNotes": data.get("altSoundNotes"),
            "altSoundVersion": data.get("altSoundVersionOverride"),
            "altSoundArchiveFormat": data.get("altSoundArchiveFormat"),
            "altSoundArchiveRoot": data.get("altSoundArchiveRoot"),
            "altSoundNSFW": data.get("altSoundNSFW"),
            "applyFixes": data.get("applyFixes"),
            "backglassAuthors": data.get("backglassAuthorsOverride"),
            "backglassBundled": data.get("backglassBundled"),
            "backglassChecksum": backglassChecksum,
            "backglassFileUrl": as_url_list(data.get("backglassUrlOverride")),
            "backglassImage": data.get("backglassImageOverride"),
            "backglassNotes": data.get("backglassNotes"),
            "backglassNSFW": data.get("backglassNSFW"),
            "coloredROMBundled": data.get("coloredROMBundled"),
            "coloredROMChecksum": coloredROMChecksum,
            "coloredROMFileUrl": as_url_list(data.get("coloredROMUrlOverride")),
            "coloredROMNotes": data.get("coloredROMNotes"),
            "coloredROMPin2DMD": data.get("coloredROMPin2DMD"),
            "coloredROMVersion": data.get("coloredROMVersionOverride"),
            "coloredROMNSFW": data.get("coloredROMNSFW"),
            "diffAuthors": data.get("diffAuthorsOverride"),
            "diffFileUrl": as_url_list(data.get("diffUrlOverride")),
            "diffNotes": data.get("diffNotes"),
            "diffVersion": data.get("diffVersionOverride"),
            "diffChecksum": diffChecksum,
            "diffNSFW": data.get("diffNSFW"),
            "enabled": data.get("enabled"),
            # NSFW/adult-content flags (author-set). "nsfw" is table-wide; the
            # per-component *NSFW flags let the wizard hide or filter specific
            # content when the user hasn't opted into NSFW. The vpx/playfield flag
            # is "vpxNSFW" in table.yml (matching vpxChecksum) but surfaces as
            # "tableNSFW" in the manifest (matching tableChecksum).
            "nsfw": data.get("nsfw"),
            "tableNSFW": data.get("vpxNSFW"),
            "fps": data.get("fps"),
            "mainNotes": data.get("mainNotes"),
            "name": data.get("tableNameOverride"),
            "manufacturer": data.get("tableManufacturerOverride"),
            "year": data.get("tableYearOverride"),
            "pupArchiveRoot": data.get("pupArchiveRoot"),
            "pupBundled": data.get("pupBundled"),
            "pupChecksum": pupChecksum,
            "pupFileUrl": as_url_list(data.get("pupFileUrl")),
            "pupNotes": data.get("pupNotes"),
            "pupRequired": data.get("pupRequired"),
            "pupVersion": data.get("pupVersion"),
            "pupArchiveFormat": data.get("pupArchiveFormat"),
            "pupNSFW": data.get("pupNSFW"),
            "romBundled": data.get("romBundled"),
            "romChecksum": romChecksum,
            "romFileUrl": as_url_list(data.get("romUrlOverride")),
            "romNotes": data.get("romNotes"),
            "romVersion": data.get("romVersionOverride"),
            "romNSFW": data.get("romNSFW"),
            "tableChecksum": vpxChecksum,
            "tableNotes": data.get("tableNotes"),
            "tagline": data.get("tagline"),
            "testers": data.get("testers"),
            "vpsdbId": data.get("tableVPSId"),
            # Per-component VPS *file* ids (vpsdbId above is the table/game id).
            # These name the exact vpsdb file a component was resolved from, so a
            # broken-link report can point the service at the right file's urls.
            "tableVpsId": vpxVPSId,
            "backglassVpsId": backglassVPSId,
            "romVpsId": romVPSId,
            "pupVpsId": pupVPSId,
            "altSoundVpsId": altSoundVPSId,
            "coloredROMVpsId": coloredROMVPSId,
            "diffVpsId": diffVPSId,
        }
        if tableVPSId:
            table = vpsdb.get_table(tableVPSId)
            if not table:
                not_found(folder_name, "Table", tableVPSId)
                continue

            # Entry-level broken: upstream has flagged this whole game as
            # problematic, so skip it (distinct reason from a dead link).
            if table.get("broken"):
                skip(folder_name, "broken_entry", f"vpsdb entry marked broken ({tableVPSId})")
                continue

            table_meta["designers"] = table.get("designers", [])
            table_meta["image"] = table.get("imgUrl", "")

            if not table_meta["name"]:
                table_meta["name"] = table.get("name", "")

            if not table_meta["manufacturer"]:
                table_meta["manufacturer"] = table.get("manufacturer", "")

            if not table_meta["year"]:
                table_meta["year"] = table.get("year", 0)

            table_meta["players"] = table.get("players", 0)
            table_meta["type"] = table.get("type", "")
            table_meta["version"] = table.get("version", "")

        if vpxVPSId:
            tableFile = vpsdb.get_tablefile_by_id(vpxVPSId)
            if tableFile:
                print(f"Parsing table {vpxVPSId} for {folder_name}")
                table_meta["tableAuthors"] = tableFile.get("authors", [])
                table_meta["tableComment"] = tableFile.get("comment", "")
                table_meta["tableImage"] = tableFile.get("imgUrl", "")
                table_meta["tableVersion"] = tableFile.get("version", "")
                # The vpx table file is always essential.
                if not resolve_essential_urls(
                    folder_name, table_meta, "tableFileUrl", tableFile.get("urls"), vpxVPSId
                ):
                    continue
            else:
                not_found(folder_name, "VPX id", vpxVPSId)
                continue
        else:
            print(f"{error_prefix}: Table {folder_name} missing VPX file")
            if warn_on_error:
                skip(folder_name, "not_found", "missing VPX file (no vpxVPSId)")
                continue
            else:
                sys.exit(1)

        if backglassVPSId:
            backglass = vpsdb.get_backglass_by_id(backglassVPSId)
            if backglass:
                print(f"Parsing backglass {backglassVPSId} for {folder_name}")
                table_meta["backglassAuthors"] = backglass.get("authors", [])
                table_meta["backglassComment"] = backglass.get("comment", "")
                table_meta["backglassImage"] = backglass.get("imgUrl", "")
                table_meta["backglassVersion"] = backglass.get("version", "")
                if not resolve_essential_urls(
                    folder_name, table_meta, "backglassFileUrl", backglass.get("urls"), backglassVPSId
                ):
                    continue
            else:
                not_found(folder_name, "Backglass id", backglassVPSId)
                continue

        if diffVPSId:
            diff = vpsdb.get_diff_by_id(diffVPSId)
            if diff:
                print(f"Parsing diff {diffVPSId} for {folder_name}")

                if not table_meta["diffAuthors"]:
                    table_meta["diffAuthors"] = diff.get("authors", [])
                if not table_meta["diffVersion"]:
                    table_meta["diffVersion"] = diff.get("version", "")
                if not resolve_essential_urls(
                    folder_name, table_meta, "diffFileUrl", diff.get("urls"), diffVPSId,
                    override_wins=True,
                ):
                    continue
            else:
                not_found(folder_name, "diff id", diffVPSId)
                continue

        if pupVPSId:
            pup = vpsdb.get_pup_by_id(pupVPSId)
            if pup:
                print(f"Parsing PUP PACK {pupVPSId} for {folder_name}")
                table_meta["pupAuthors"] = pup.get("authors", [])
                table_meta["pupComment"] = pup.get("comment", "")
                if not resolve_essential_urls(
                    folder_name, table_meta, "pupFileUrl", pup.get("urls"), pupVPSId
                ):
                    continue
            else:
                not_found(folder_name, "PUP PACK id", pupVPSId)
                continue

        if romVPSId:
            rom = vpsdb.get_rom_by_id(romVPSId)
            if rom:
                print(f"Parsing ROM {romVPSId} for {folder_name}")
                table_meta["romAuthors"] = rom.get("authors", [])
                table_meta["romComment"] = rom.get("comment", "")
                if not table_meta["romVersion"]:
                    table_meta["romVersion"] = rom.get("version", "")
                if not resolve_essential_urls(
                    folder_name, table_meta, "romFileUrl", rom.get("urls"), romVPSId
                ):
                    continue
            else:
                not_found(folder_name, "ROM id", romVPSId)
                continue

        if altSoundVPSId:
            altSound = vpsdb.get_altsound_by_id(altSoundVPSId)
            if altSound:
                print(f"Parsing alt sound {altSoundVPSId} for {folder_name}")

                if not table_meta["altSoundAuthors"]:
                    table_meta["altSoundAuthors"] = altSound.get("authors", [])
                table_meta["altSoundComment"] = altSound.get("comment", "")
                if not table_meta["altSoundVersion"]:
                    table_meta["altSoundVersion"] = altSound.get("version", "")
                if not resolve_essential_urls(
                    folder_name, table_meta, "altSoundFileUrl", altSound.get("urls"), altSoundVPSId,
                    override_wins=True,
                ):
                    continue
            else:
                not_found(folder_name, "Alt sound id", altSoundVPSId)
                continue

        if coloredROMVPSId:
            coloredROM = vpsdb.get_altcolor_by_id(coloredROMVPSId)
            if coloredROM:
                print(f"Parsing colored ROM {coloredROMVPSId} for {folder_name}")
                table_meta["coloredROMAuthors"] = coloredROM.get("authors", [])
                table_meta["coloredROMComment"] = coloredROM.get("comment", "")
                table_meta["coloredROMFolder"] = coloredROM.get("folder", "")
                if not table_meta["coloredROMVersion"]:
                    table_meta["coloredROMVersion"] = coloredROM.get("version", "")
                if not resolve_essential_urls(
                    folder_name, table_meta, "coloredROMFileUrl", coloredROM.get("urls"), coloredROMVPSId
                ):
                    continue
            else:
                not_found(folder_name, "Colored ROM id", coloredROMVPSId)
                continue

        if tutorialVPSId:
            tutorial = vpsdb.get_tutorialfile_by_id(tutorialVPSId)
            if tutorial:
                print(f"Parsing tutorial {tutorialVPSId} for {folder_name}")
                table_meta["tutorialAuthors"] = tutorial.get("authors", [])
                table_meta["tutorialTitle"] = tutorial.get("title", "")
                table_meta["tutorialYouTubeID"] = tutorial.get("youtubeId", "")
            else:
                # The tutorial is a YouTube link, not a download, and is purely
                # optional — a missing id drops the field and warns rather than
                # skipping the table.
                warn(folder_name, f"tutorial id {tutorialVPSId} not found in VPSDB (dropped)")

        # Additional ROMs: a list of ROM objects with the same modes as the
        # primary ROM — VPSID-resolved (authors/comment/url/version from VPSDB,
        # versionOverride wins), url-override (author supplies url + version), or
        # bundled (ships inside the table download; no external URL, notes
        # required). Handled once per entry.
        additional_roms = []
        skip_table = False
        for entry in normalize_dict_list(data.get("additionalRoms")):
            vps_id = entry.get("vpsId")
            url_override = entry.get("urlOverride")
            version_override = entry.get("versionOverride")

            rom_meta = {"checksum": normalize_checksums(entry.get("checksum"))}
            if vps_id:
                rom_meta["vpsId"] = vps_id
            if entry.get("bundled"):
                rom_meta["bundled"] = True
            if entry.get("notes"):
                rom_meta["notes"] = entry.get("notes")

            if vps_id:
                rom = vpsdb.get_rom_by_id(vps_id)
                if rom:
                    print(f"Parsing additional ROM {vps_id} for {folder_name}")
                    rom_meta["authors"] = rom.get("authors", [])
                    rom_meta["comment"] = rom.get("comment", "")
                    rom_meta["version"] = version_override or rom.get("version", "")
                    urls = pick_urls(rom.get("urls"))
                    if urls:
                        rom_meta["fileUrl"] = urls
                    elif not rom_meta.get("bundled"):
                        # An additional ROM is optional to declare, but once
                        # declared the table needs it — a non-bundled one with no
                        # usable link makes the table unusable.
                        skip(
                            folder_name,
                            "broken_link",
                            f"additionalRom {vps_id}: all urls broken",
                        )
                        skip_table = True
                        break
                    # bundled: ships in the table download, no external url needed
                else:
                    if not warn_on_error:
                        print(
                            f"{error_prefix}: Additional ROM id {vps_id} not found in VPSDB"
                        )
                        sys.exit(1)
                    skip(
                        folder_name,
                        "not_found",
                        f"additional ROM {vps_id} not found in VPSDB",
                    )
                    skip_table = True
                    break
            elif url_override:
                rom_meta["fileUrl"] = as_url_list(url_override)
                rom_meta["version"] = version_override

            additional_roms.append(rom_meta)

        if skip_table:
            continue

        table_meta["additionalRoms"] = additional_roms or None

        tables[folder_name] = table_meta

    write_summary(tables, skipped, warnings)
    return tables
