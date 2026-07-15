import json
import re
import sys
from pathlib import Path

import requests
import yaml


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


def get_table_meta(files, warn_on_error=True):
    error_prefix = "ERROR"
    if warn_on_error:
        error_prefix = "WARNING"

    tables = {}
    vpsdb = VPSDB()

    for table_yaml in files:
        path = Path(table_yaml)
        folder_name = path.parent.name

        print(f"Processing {folder_name}")
        with open(table_yaml, "r") as table_data:
            data = yaml.safe_load(table_data)

        altSoundVPSId = data.get("altSoundVPSId")
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
            "altSoundChecksum": altSoundChecksum,
            "applyFixes": data.get("applyFixes"),
            "backglassAuthors": data.get("backglassAuthorsOverride"),
            "backglassBundled": data.get("backglassBundled"),
            "backglassChecksum": backglassChecksum,
            "backglassFileUrl": data.get("backglassUrlOverride"),
            "backglassImage": data.get("backglassImageOverride"),
            "backglassNotes": data.get("backglassNotes"),
            "coloredROMBundled": data.get("coloredROMBundled"),
            "coloredROMChecksum": coloredROMChecksum,
            "coloredROMFileUrl": data.get("coloredROMUrlOverride"),
            "coloredROMNotes": data.get("coloredROMNotes"),
            "coloredROMPin2DMD": data.get("coloredROMPin2DMD"),
            "coloredROMVersion": data.get("coloredROMVersionOverride"),
            "diffAuthors": data.get("diffAuthorsOverride"),
            "diffFileUrl": data.get("diffUrlOverride"),
            "diffNotes": data.get("diffNotes"),
            "diffVersion": data.get("diffVersionOverride"),
            "diffChecksum": diffChecksum,
            "enabled": data.get("enabled"),
            "fps": data.get("fps"),
            "mainNotes": data.get("mainNotes"),
            "name": data.get("tableNameOverride"),
            "manufacturer": data.get("tableManufacturerOverride"),
            "year": data.get("tableYearOverride"),
            "pupArchiveRoot": data.get("pupArchiveRoot"),
            "pupBundled": data.get("pupBundled"),
            "pupChecksum": pupChecksum,
            "pupFileUrl": data.get("pupFileUrl"),
            "pupNotes": data.get("pupNotes"),
            "pupRequired": data.get("pupRequired"),
            "pupVersion": data.get("pupVersion"),
            "pupArchiveFormat": data.get("pupArchiveFormat"),
            "romBundled": data.get("romBundled"),
            "romChecksum": romChecksum,
            "romFileUrl": data.get("romUrlOverride"),
            "romNotes": data.get("romNotes"),
            "romVersion": data.get("romVersionOverride"),
            "tableChecksum": vpxChecksum,
            "tableNotes": data.get("tableNotes"),
            "tagline": data.get("tagline"),
            "testers": data.get("testers"),
            "vpsdbId": data.get("tableVPSId"),
        }
        if tableVPSId:
            table = vpsdb.get_table(tableVPSId)
            if not table:
                print(f"{error_prefix}: Table {tableVPSId} not found in VPSDB")
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)

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
                table_meta["tableFileUrl"] = tableFile.get("urls", [])[0].get("url", "")
                table_meta["tableImage"] = tableFile.get("imgUrl", "")
                table_meta["tableVersion"] = tableFile.get("version", "")
            else:
                print(f"{error_prefix}: VPX id {vpxVPSId} not found in VPSDB")
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)
        else:
            print(f"{error_prefix}: Table {folder_name} missing VPX file")
            if warn_on_error:
                print(f"WARNING: Skipping {folder_name}")
                continue
            else:
                sys.exit(1)

        if backglassVPSId:
            backglass = vpsdb.get_backglass_by_id(backglassVPSId)
            if backglass:
                print(f"Parsing backglass {backglassVPSId} for {folder_name}")
                table_meta["backglassAuthors"] = backglass.get("authors", [])
                table_meta["backglassComment"] = backglass.get("comment", "")
                table_meta["backglassFileUrl"] = backglass.get("urls", [])[0].get(
                    "url", ""
                )
                table_meta["backglassImage"] = backglass.get("imgUrl", "")
                table_meta["backglassVersion"] = backglass.get("version", "")
            else:
                print(
                    f"{error_prefix}: Backglass id {backglassVPSId} not found in VPSDB"
                )
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)

        if diffVPSId:
            diff = vpsdb.get_diff_by_id(diffVPSId)
            if diff:
                print(f"Parsing diff {diffVPSId} for {folder_name}")

                if not table_meta["diffAuthors"]:
                    table_meta["diffAuthors"] = diff.get("authors", [])
                if not table_meta["diffFileUrl"]:
                    table_meta["diffFileUrl"] = diff.get("urls", [])[0].get("url", "")
                if not table_meta["diffVersion"]:
                    table_meta["diffVersion"] = diff.get("version", "")
            else:
                print(f"{error_prefix}: diff id {diffVPSId} not found in VPSDB")
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)
            
        if pupVPSId:
            pup = vpsdb.get_pup_by_id(pupVPSId)
            if pup:
                print(f"Parsing PUP PACK {pupVPSId} for {folder_name}")
                table_meta["pupAuthors"] = pup.get("authors", [])
                table_meta["pupComment"] = pup.get("comment", "")
                table_meta["pupFileUrl"] = pup.get("urls", [])[0].get("url", "")
            else:
                print(f"{error_prefix}: PUP PACK id {pupVPSId} not found in VPSDB")
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)
                    
        if romVPSId:
            rom = vpsdb.get_rom_by_id(romVPSId)
            if rom:
                print(f"Parsing ROM {romVPSId} for {folder_name}")
                table_meta["romAuthors"] = rom.get("authors", [])
                table_meta["romComment"] = rom.get("comment", "")
                table_meta["romFileUrl"] = rom.get("urls", [])[0].get("url", "")
                if not table_meta["romVersion"]:
                    table_meta["romVersion"] = rom.get("version", "")
            else:
                print(f"{error_prefix}: ROM id {romVPSId} not found in VPSDB")
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)

        if altSoundVPSId:
            altSound = vpsdb.get_altsound_by_id(altSoundVPSId)
            if altSound:
                print(f"Parsing alt sound {altSoundVPSId} for {folder_name}")
                table_meta["altSoundAuthors"] = altSound.get("authors", [])
                table_meta["altSoundComment"] = altSound.get("comment", "")
                table_meta["altSoundFileUrl"] = altSound.get("urls", [])[0].get(
                    "url", ""
                )
                if not table_meta["altSoundVersion"]:
                    table_meta["altSoundVersion"] = altSound.get("version", "")
            else:
                print(
                    f"{error_prefix}: Alt sound id {altSoundVPSId} not found in VPSDB"
                )
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)

        if coloredROMVPSId:
            coloredROM = vpsdb.get_altcolor_by_id(coloredROMVPSId)
            if coloredROM:
                print(f"Parsing colored ROM {coloredROMVPSId} for {folder_name}")
                table_meta["coloredROMAuthors"] = coloredROM.get("authors", [])
                table_meta["coloredROMComment"] = coloredROM.get("comment", "")
                table_meta["coloredROMFolder"] = coloredROM.get("folder", "")
                table_meta["coloredROMFileUrl"] = coloredROM.get("urls", [])[0].get(
                    "url", ""
                )
                if not table_meta["coloredROMVersion"]:
                    table_meta["coloredROMVersion"] = coloredROM.get("version", "")
            else:
                print(
                    f"{error_prefix}: Colored ROM id {coloredROMVPSId} not found in VPSDB"
                )
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)

        if tutorialVPSId:
            tutorial = vpsdb.get_tutorialfile_by_id(tutorialVPSId)
            if tutorial:
                print(f"Parsing tutorial {tutorialVPSId} for {folder_name}")
                table_meta["tutorialAuthors"] = tutorial.get("authors", [])
                table_meta["tutorialTitle"] = tutorial.get("title", "")
                table_meta["tutorialYouTubeID"] = tutorial.get("youtubeId", "")
            else:
                print(f"{error_prefix}: Tutorial id {tutorialVPSId} not found in VPSDB")
                if warn_on_error:
                    print(f"WARNING: Skipping {folder_name}")
                    continue
                else:
                    sys.exit(1)

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
                    urls = rom.get("urls", [])
                    rom_meta["authors"] = rom.get("authors", [])
                    rom_meta["comment"] = rom.get("comment", "")
                    rom_meta["fileUrl"] = urls[0].get("url", "") if urls else ""
                    rom_meta["version"] = version_override or rom.get("version", "")
                else:
                    print(
                        f"{error_prefix}: Additional ROM id {vps_id} not found in VPSDB"
                    )
                    if warn_on_error:
                        print(f"WARNING: Skipping {folder_name}")
                        skip_table = True
                        break
                    else:
                        sys.exit(1)
            elif url_override:
                rom_meta["fileUrl"] = url_override
                rom_meta["version"] = version_override

            additional_roms.append(rom_meta)

        if skip_table:
            continue

        table_meta["additionalRoms"] = additional_roms or None

        tables[folder_name] = table_meta

    return tables
