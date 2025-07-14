import json
import sys
from pathlib import Path

import requests
import yaml


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
        pupVPSId = data.get("pupVPSId")
        romVPSId = data.get("romVPSId")
        tableVPSId = data.get("tableVPSId")
        tutorialVPSId = data.get("tutorialVPSId")
        vpxVPSId = data.get("vpxVPSId")

        altSoundChecksum = data.get("altSoundChecksum")
        backglassChecksum = data.get("backglassChecksum")
        coloredROMChecksum = data.get("coloredROMChecksum")
        romChecksum = data.get("romChecksum")
        vpxChecksum = data.get("vpxChecksum")
        pupChecksum = data.get("pupChecksum")

        if altSoundChecksum:
            altSoundChecksum = altSoundChecksum.lower()
        if backglassChecksum:
            backglassChecksum = backglassChecksum.lower()
        if coloredROMChecksum:
            coloredROMChecksum = coloredROMChecksum.lower()
        if romChecksum:
            romChecksum = romChecksum.lower()
        if vpxChecksum:
            vpxChecksum = vpxChecksum.lower()
        if pupChecksum:
            pupChecksum = pupChecksum.lower()

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
            "coloredROMVersion": data.get("coloredROMVersionOverride"),
            "enabled": data.get("enabled"),
            "fps": data.get("fps"),
            "mainNotes": data.get("mainNotes"),
            "name": data.get("tableNameOverride"),
            "manufacturer": data.get("tableManufacturerOverride"),
            "year": data.get("tableYearOverride"),
            "pupArchiveRoot": data.get("pupArchiveRoot"),
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

        tables[folder_name] = table_meta

    return tables
