#!/usr/bin/env python3
import os
import re
import sys
import time
import json
import shutil
import tempfile
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests
import vpsdb
import git
from github import Github, Auth
from github.GithubException import GithubException
from pathlib import Path


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


def get_latest_commit_hash(repo_path, folder_path):
    """
    Retrieves the latest commit hash that modified files in a specific folder.

    Args:
        repo_path (str): The path to the local Git repository.
        folder_path (str): The folder path within the repository.

    Returns:
        str: The latest commit hash, or None if no matching commits are found.
    """
    try:
        repo = git.Repo(repo_path)
        commits = list(repo.iter_commits(paths=folder_path, max_count=1))  # Only the most recent commit
        if commits:
            return commits[0].hexsha
        else:
            return None
    except git.InvalidGitRepositoryError:
        print(f"Error: Invalid Git repository at {repo_path}")
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        return None


def process_title(title, manufacturer, year):
    """
    Transforms a title for proper sorting, moving leading "The" to the end,
    and handling optional "JP's" or "JPs" prefixes, moving them after the comma
    when 'The' is not present.
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


def fetch_existing_manifest(github_token, repo_name, release_tag):
    """
    Fetch manifest.json (if present) from the target release and return it as a dict.
    Keys are expected to be table folder names; values include 'configVersion'.
    """
    try:
        auth = Auth.Token(github_token)
        g = Github(auth=auth)
        repo = g.get_repo(repo_name)
        rel = repo.get_release(release_tag)
        for asset in rel.get_assets():
            if asset.name == "manifest.json":
                url = asset.browser_download_url
                headers = {
                    "Authorization": f"token {github_token}",
                    "Accept": "application/octet-stream",
                }
                r = requests.get(url, headers=headers, timeout=30)
                r.raise_for_status()
                print("[INFO] Loaded previous manifest.json from release")
                return json.loads(r.text)
    except Exception as e:
        print(f"[INFO] No previous manifest found or failed to load it: {e}")
    return {}


def build_asset_index(release):
    """
    Build a one-time index of existing assets to avoid repeated pagination.
    """
    by_name = {}
    url_by_name = {}
    for asset in release.get_assets():
        by_name[asset.name] = asset
        url_by_name[asset.name] = asset.browser_download_url
    return {"by_name": by_name, "url_by_name": url_by_name}


def upload_release_asset(github_token, repo_name, release, asset_index, index_lock, file_path, clobber=True, max_attempts=3):
    """
    Uploads a file as a release asset using PyGithub. Returns the browser_download_url or None.
    Uses a shared asset_index (name -> asset) to avoid repeated listing/pagination.
    """
    file_name = os.path.basename(file_path)
    try:
        # Optional clobber of existing asset (by name) using cached index
        if clobber:
            with index_lock:
                asset = asset_index["by_name"].get(file_name)
            if asset is not None:
                try:
                    print(f"[INFO] Deleting existing asset '{file_name}'...")
                    asset.delete_asset()
                    # update cache
                    with index_lock:
                        asset_index["by_name"].pop(file_name, None)
                        asset_index["url_by_name"].pop(file_name, None)
                    time.sleep(0.2)
                except GithubException as ge:
                    print(f"[WARN] Could not delete existing asset ({ge.status}): {ge.data}")

        # If after optional clobber, asset still exists -> skip
        with index_lock:
            if file_name in asset_index["by_name"]:
                print(f"[INFO] Asset '{file_name}' already exists in release. Skipping upload.")
                return asset_index["url_by_name"].get(file_name)

        # Upload with correct content type (zip) and stable name
        attempt = 0
        while attempt < max_attempts:
            attempt += 1
            try:
                print(f"[INFO] Uploading '{file_name}' (attempt {attempt}/{max_attempts})...")
                # PyGithub signature: upload_asset(path, label=None, name=None, content_type='application/octet-stream')
                asset = release.upload_asset(
                    file_path,
                    label=file_name,
                    name=file_name,
                    content_type="application/zip" if file_name.endswith(".zip") else "application/octet-stream",
                )
                print(f"[INFO] Uploaded {file_name} to release.")
                # update cache without re-listing
                with index_lock:
                    asset_index["by_name"][file_name] = asset
                    asset_index["url_by_name"][file_name] = asset.browser_download_url
                return asset.browser_download_url
            except GithubException as ge:
                if ge.status == 403:
                    msg = ge.data if isinstance(ge.data, dict) else str(ge.data)
                    print(f"[ERROR] 403 Forbidden while uploading '{file_name}': {msg}")
                    print(
                        "HINTS: "
                        "1) Ensure workflow has `permissions: contents: write` "
                        "2) Ensure repo setting 'Workflow permissions' is 'Read and write' "
                        "3) Ensure you're uploading to the SAME repo the workflow runs in "
                        "4) Fine-grained PAT needed if targeting a different repo"
                    )
                    break
                elif ge.status in (502, 503, 504):
                    print(f"[WARN] Transient server error ({ge.status}); will retry.")
                    time.sleep(2 ** attempt)
                else:
                    print(f"[ERROR] Upload failed ({ge.status}): {ge.data}")
                    break
            except Exception as e:
                print(f"[ERROR] Unexpected upload error: {e}")
                break

        return None
    except Exception as e:
        print(f"[ERROR] upload_release_asset fatal: {e}")
        return None


def process_table(args):
    # Unpack for ThreadPoolExecutor compatibility
    table, table_data, github_token, repo_name, release, asset_index, index_lock = args
    external_path = os.path.join("external", table)
    result = table, table_data.copy()
    if not os.path.isdir(external_path):
        print(f"Warning: Directory {external_path} does not exist, skipping {table}.")
        return result

    # Get latest commit for this folder
    config_version = get_latest_commit_hash(".", external_path)
    if not config_version:
        print(f"Error: No commit found for {external_path}, skipping {table}.")
        return result

    # Zip the table directory in a temp folder
    with tempfile.TemporaryDirectory() as tmpdir:
        zip_base = os.path.join(tmpdir, table)
        try:
            shutil.make_archive(zip_base, "zip", external_path)
            zip_path = zip_base + ".zip"
            print(f"Uploading {zip_path} to GitHub...")
            download_url = upload_release_asset(
                github_token, repo_name, release, asset_index, index_lock, zip_path
            )
            if download_url:
                new_data = result[1]
                new_data["repoConfig"] = download_url
                new_data["configVersion"] = config_version[:7]
                new_data["name"] = process_title(new_data["name"], new_data["manufacturer"], new_data["year"])
                result = (table, new_data)
            else:
                print(f"Failed to upload asset for {table}")
        except Exception as e:
            print(f"Error processing {table}: {e}")
    return result


def main():
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name = os.environ.get("GITHUB_REPOSITORY")
    release_tag = os.environ.get("GITHUB_REF_NAME")

    if not github_token or not repo_name or not release_tag:
        print("Error: Required environment variables not set.")
        sys.exit(1)

    # Sanity probe: ensure we can reach the release
    try:
        g = Github(auth=Auth.Token(github_token))
        repo = g.get_repo(repo_name)
        rel = repo.get_release(release_tag)

        # Capability probe: listing assets should succeed with a write-capable token
        _ = list(rel.get_assets())
    except GithubException as ge:
        if ge.status == 403:
            print("[ERROR] Token cannot access the release. Likely missing 'contents: write' or repo workflow perms set to read-only.")
        elif ge.status == 404:
            print(f"[ERROR] Release '{release_tag}' not found in '{repo_name}'.")
        else:
            print(f"[ERROR] Unable to access release ({ge.status}): {ge.data}")
        sys.exit(1)
    except Exception as e:
        print(f"[ERROR] Unexpected error while probing release access: {e}")
        sys.exit(1)

    # Discover tables from table.yml files
    files = find_table_yml()
    tables = vpsdb.get_table_meta(files)

    # Remove disabled tables before processing
    tables_to_remove = [table for table, data in tables.items() if data.get("enabled") is False]
    for table in tables_to_remove:
        print(f"Skipping disabled table: {table}")
        del tables[table]

    # Load previous manifest (if any) to enable unchanged-skip
    prev_manifest = fetch_existing_manifest(github_token, repo_name, release_tag)

    # Decide which tables changed by comparing latest commit short hash with manifest configVersion
    changed_tables = {}
    unchanged_tables = []
    for table, data in list(tables.items()):
        external_path = os.path.join("external", table)
        latest = get_latest_commit_hash(".", external_path)
        short = (latest or "")[:7]
        prev_short = ""
        if isinstance(prev_manifest, dict):
            prev_short = (prev_manifest.get(table, {}) or {}).get("configVersion", "")
        if short and short == prev_short:
            unchanged_tables.append(table)
        else:
            changed_tables[table] = data

    if unchanged_tables:
        print(f"[INFO] Skipping unchanged tables: {', '.join(sorted(unchanged_tables))}")

    if not changed_tables:
        print("[INFO] No changed tables detected; nothing to build.")
        # Keep release consistent: upload the previous manifest if it exists
        if isinstance(prev_manifest, dict) and prev_manifest:
            manifest_file = "manifest.json"
            with open(manifest_file, "w") as f:
                json.dump(prev_manifest, f, indent=2)
            # Build asset index once and upload manifest using it
            asset_index = build_asset_index(rel)
            index_lock = threading.Lock()
            _ = upload_release_asset(github_token, repo_name, rel, asset_index, index_lock, manifest_file, clobber=True)
            try:
                os.remove(manifest_file)
            except OSError:
                pass
        sys.exit(0)

    # Build asset index once to avoid pagination per file
    asset_index = build_asset_index(rel)
    index_lock = threading.Lock()

    # Prepare arguments for parallel processing (only changed)
    pool_args = [
        (table, changed_tables[table], github_token, repo_name, rel, asset_index, index_lock)
        for table in changed_tables
    ]

    # Process tables in parallel (adjust max_workers as needed)
    updated_tables = {}
    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = {executor.submit(process_table, arg): arg[0] for arg in pool_args}
        for future in as_completed(futures):
            table, updated_data = future.result()
            updated_tables[table] = updated_data

    # Merge manifest: keep previous entries for unchanged tables, update changed ones
    merged_manifest = dict(prev_manifest) if isinstance(prev_manifest, dict) else {}
    merged_manifest.update(updated_tables)

    # Write & upload manifest
    manifest_file = "manifest.json"
    with open(manifest_file, "w") as f:
        json.dump(merged_manifest, f, indent=2)

    manifest_url = upload_release_asset(github_token, repo_name, rel, asset_index, index_lock, manifest_file, clobber=True)
    print(f"Uploaded manifest.json to release: {manifest_url}")

    # Optional cleanup
    try:
        os.remove(manifest_file)
    except OSError:
        pass


if __name__ == "__main__":
    main()
