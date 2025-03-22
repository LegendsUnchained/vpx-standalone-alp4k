import os
import requests
import shutil
import json
import yaml

from github import Github
from pathlib import Path


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

    def get_backglass_by_id(self, id):
        for table in self.tables:
            if "b2sFiles" in table:
                for b2sFile in table["b2sFiles"]:
                    if b2sFile.get("id") == id:
                        return b2sFile
        return self.get_tablefile_by_id(id)

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


def find_table_yml(base_dir="external"):
    result = []
    if not os.path.exists(base_dir):
        print(f"Directory {base_dir} does not exist.")
        return result

    for entry in os.listdir(base_dir):
        entry_path = os.path.join(base_dir, entry)
        if os.path.isdir(entry_path) and entry.startswith("vpx-"):
            table_yml = os.path.join(entry_path, "table.yml")
            if os.path.exists(table_yml):
                result.append(table_yml)

    return result


def upload_release_asset(github_token, repo_name, release_tag, file_path, clobber=True):
    """Uploads a file as a release asset."""

    try:
        g = Github(github_token)
        repo = g.get_repo(repo_name)
        release = repo.get_release(release_tag)

        file_name = os.path.basename(file_path)

        # Check if the asset already exists and clobber if needed
        existing_assets = release.get_assets()
        for asset in existing_assets:
            if asset.name == file_name and clobber:
                asset.delete_asset()
                break

        asset = release.upload_asset(file_path, label=file_name)
        print(f"Uploaded {file_name} to release {release_tag}")

        release = repo.get_release(release_tag)  # Refresh the release object

        for asset in release.get_assets():
            if asset.name == file_name:
                browser_download_url = asset.browser_download_url
                return browser_download_url
                break

    except Exception as e:
        print(f"Error uploading asset: {e}")
        exit(1)


if __name__ == "__main__":
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name = os.environ.get("GITHUB_REPOSITORY")
    release_tag = os.environ.get("GITHUB_REF_NAME")
    if not github_token or not repo_name or not release_tag:
        print("Error: Required environment variables not set.")
        exit(1)

    tables = {}
    files = find_table_yml()

    vpsdb = VPSDB()

    for table_yaml in files:
        path = Path(table_yaml)
        folder_name = path.parent.name

        print(f"Processing {folder_name}")
        with open(table_yaml, "r") as table_data:
            data = yaml.safe_load(table_data)

        tableVPSId = data.get("tableVPSId")
        vpxVPSId = data.get("vpxVPSId")
        backglassVPSId = data.get("backglassVPSId")
        romVPSId = data.get("romVPSId")
        coloredROMVPSId = data.get("coloredROMVPSId")
        tutorialVPSId = data.get("tutorialVPSId")

        backglassChecksum = data.get("backglassChecksum")
        coloredROMChecksum = data.get("coloredROMChecksum")
        romChecksum = data.get("romChecksum")
        vpxChecksum = data.get("vpxChecksum")

        if backglassChecksum:
            backglassChecksum = backglassChecksum.lower()
        if coloredROMChecksum:
            coloredROMChecksum = coloredROMChecksum.lower()
        if romChecksum:
            romChecksum = romChecksum.lower()
        if vpxChecksum:
            vpxChecksum = vpxChecksum.lower()

        table_meta = {
            "applyFixes": data.get("applyFixes"),
            "backglassChecksum": backglassChecksum,
            "backglassNotes": data.get("backglassNotes"),
            "coloredROMChecksum": coloredROMChecksum,
            "coloredROMNotes": data.get("coloredROMNotes"),
            "fps": data.get("fps"),
            "mainNotes": data.get("mainNotes"),
            "romChecksum": romChecksum,
            "romNotes": data.get("romNotes"),
            "romFileUrl": data.get("romUrlOverride"),
            "tableChecksum": vpxChecksum,
            "tableNotes": data.get("tableNotes"),
            "tagline": data.get("tagline"),
            "testers": data.get("testers"),
        }
        if tableVPSId:
            table = vpsdb.get_table(tableVPSId)
            if not table:
                print(f"WARNING: Table {tableVPSId} not found in VPSDB")
                print(f"WARNING: Skipping {folder_name}")
                continue

            table_meta["designers"] = table.get("designers", [])
            table_meta["image"] = table.get("imgUrl", "")
            table_meta["manufacturer"] = table.get("manufacturer", "")
            table_meta["name"] = table.get("name", "")
            table_meta["players"] = table.get("players", 0)
            table_meta["type"] = table.get("type", "")
            table_meta["version"] = table.get("version", "")
            table_meta["year"] = table.get("year", 0)

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
                print(f"WARNING: VPX id {vpxVPSId} not found in VPSDB")
                print(f"WARNING: Skipping {folder_name}")
                continue
        else:
            print(f"WARNING: Table {folder_name} missing VPX file")
            print(f"WARNING: Skipping {folder_name}")
            continue

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
                print(f"WARNING: Backglass id {backglassVPSId} not found in VPSDB")
                print(f"WARNING: Skipping {folder_name}")
                continue

        if romVPSId:
            rom = vpsdb.get_rom_by_id(romVPSId)
            if rom:
                print(f"Parsing ROM {romVPSId} for {folder_name}")
                table_meta["romAuthors"] = rom.get("authors", [])
                table_meta["romComment"] = rom.get("comment", "")
                table_meta["romFileUrl"] = rom.get("urls", [])[0].get("url", "")
                table_meta["romVersion"] = rom.get("version", "")
            else:
                print(f"WARNING: ROM id {romVPSId} not found in VPSDB")
                print(f"WARNING: Skipping {folder_name}")
                continue

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
                table_meta["coloredROMVersion"] = coloredROM.get("version", "")
            else:
                print(f"WARNING: Colored ROM id {coloredROMVPSId} not found in VPSDB")
                print(f"WARNING: Skipping {folder_name}")
                continue

        if tutorialVPSId:
            tutorial = vpsdb.get_tutorialfile_by_id(tutorialVPSId)
            if tutorial:
                print(f"Parsing tutorial {tutorialVPSId} for {folder_name}")
                table_meta["tutorialAuthors"] = tutorial.get("authors", [])
                table_meta["tutorialTitle"] = tutorial.get("title", "")
                table_meta["tutorialYouTubeID"] = tutorial.get("youtubeId", "")
            else:
                print(f"WARNING: Tutorial id {tutorialVPSId} not found in VPSDB")
                print(f"WARNING: Skipping {folder_name}")
                continue

        external_path = str(path.parent)

        print(f"Zipping {folder_name} for release")
        shutil.make_archive(folder_name, "zip", external_path)

        print(f"Uploading {folder_name}.zip to GitHub")
        file_path = f"{folder_name}.zip"
        download_url = upload_release_asset(
            github_token, repo_name, release_tag, file_path
        )

        print(f"Cleaning up {folder_name}.zip")
        os.remove(file_path)

        table_meta["repoConfig"] = download_url

        tables[folder_name] = table_meta

    manifest_file = "manifest.json"
    json.dump(tables, open(manifest_file, "w"))
    upload_release_asset(github_token, repo_name, release_tag, manifest_file)
