import os
import re
import shutil
import json
import tempfile
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import vpsdb
import git
from github import Github, GithubException
from pathlib import Path

def find_table_yml(base_dir="external"):
    result = []
    if not os.path.exists(base_dir):
        print(f"Directory {base_dir} does not exist.")
        return result
    for entry in os.listdir(base_dir):
        entry_path = os.path.join(base_dir, entry)
        if os.path.isdir(entry_path) and entry.startswith("vpx-"):
            yml = os.path.join(entry_path, "table.yml")
            if os.path.exists(yml):
                result.append(yml)
    return result

def get_latest_commit_hash(repo_path, folder_path):
    try:
        repo = git.Repo(repo_path)
        commits = list(repo.iter_commits(paths=folder_path, max_count=1))
        return commits[0].hexsha if commits else None
    except git.InvalidGitRepositoryError:
        print(f"Error: Invalid Git repository at {repo_path}")
    except Exception as e:
        print(f"Error getting commit hash: {e}")
    return None

def process_title(title, manufacturer, year):
    name = ""
    match_the = re.match(r"^(JP'?s\s*)?(The)\s+(.+)$", title)
    match_jps = re.match(r"^(JP'?s)\s+(.+)$", title)
    if match_the:
        prefix = match_the.group(1) or ""
        name = f"{match_the.group(3)}, {prefix}{match_the.group(2)}"
    elif match_jps:
        name = f"{match_jps.group(2)}, {match_jps.group(1)}"
    else:
        name = title
    return f"{name} ({manufacturer} {year})"

def upload_release_asset(github_token, repo_name, release_tag, file_path,
                         existing_assets=None, clobber=True):
    """
    Upload a file to the GitHub release, using a cached asset list to avoid
    repeated GET /assets calls.
    """
    try:
        g = Github(github_token)
        repo = g.get_repo(repo_name)
        release = repo.get_release(release_tag)

        # Use cached list if provided, otherwise fetch once
        assets = existing_assets if existing_assets is not None else list(release.get_assets())

        file_name = os.path.basename(file_path)
        if clobber:
            for asset in assets:
                if asset.name == file_name:
                    asset.delete_asset()
                    break

        asset = release.upload_asset(file_path, label=file_name)
        print(f"Uploaded {file_name} to release {release_tag}")

        # small pause to avoid abuse detection
        time.sleep(0.5)

        # Refresh and return download URL
        release = repo.get_release(release_tag)
        for asset in release.get_assets():
            if asset.name == file_name:
                return asset.browser_download_url

    except GithubException as e:
        if e.status == 403:
            print("Error: 403 Forbidden when uploading asset. "
                  "Are your token scopes set to repo (or public_repo for public repos)?")
        else:
            print(f"GitHub API error: {e}")
    except Exception as e:
        print(f"Error uploading asset: {e}")
    return None

def process_table(args):
    table, table_data, github_token, repo_name, release_tag, all_assets = args
    external_path = os.path.join("external", table)
    result = (table, table_data.copy())

    if not os.path.isdir(external_path):
        print(f"Warning: Directory {external_path} does not exist, skipping {table}.")
        return result

    config_version = get_latest_commit_hash(".", external_path)
    if not config_version:
        print(f"Error: No commit found for {external_path}, skipping {table}.")
        return result

    with tempfile.TemporaryDirectory() as tmpdir:
        zip_base = os.path.join(tmpdir, table)
        try:
            shutil.make_archive(zip_base, "zip", external_path)
            zip_path = zip_base + ".zip"
            print(f"Uploading {zip_path} to GitHub...")

            download_url = upload_release_asset(
                github_token, repo_name, release_tag,
                zip_path, existing_assets=all_assets
            )
            if download_url:
                new_data = result[1]
                new_data["repoConfig"] = download_url
                new_data["configVersion"] = config_version[:7]
                new_data["name"] = process_title(
                    new_data["name"],
                    new_data["manufacturer"],
                    new_data["year"]
                )
                result = (table, new_data)
            else:
                print(f"Failed to upload asset for {table}")

        except Exception as e:
            print(f"Error processing {table}: {e}")

    return result

if __name__ == "__main__":
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name    = os.environ.get("GITHUB_REPOSITORY")
    release_tag  = os.environ.get("GITHUB_REF_NAME")

    if not (github_token and repo_name and release_tag):
        print("Error: Required environment variables not set.")
        sys.exit(1)

    # Step 1: verify token has access to the repo
    g = Github(github_token)
    try:
        repo = g.get_repo(repo_name)
    except GithubException as e:
        if e.status == 403:
            print("Error: 403 Forbidden. Ensure GITHUB_TOKEN has the correct scopes "
                  "(repo or public_repo).")
        else:
            print(f"Error accessing repository: {e}")
        sys.exit(1)

    # Get the release object
    try:
        release = repo.get_release(release_tag)
    except Exception as e:
        print(f"Error fetching release {release_tag}: {e}")
        sys.exit(1)

    # Step 2: cache the full asset list once
    all_assets = list(release.get_assets())

    # Gather tables
    files = find_table_yml()
    tables = vpsdb.get_table_meta(files)
    # Remove disabled
    for tbl in [t for t, d in tables.items() if not d.get("enabled")]:
        print(f"Skipping disabled table: {tbl}")
        tables.pop(tbl, None)

    # Prepare args (now includes all_assets)
    pool_args = [
        (tbl, tables[tbl], github_token, repo_name, release_tag, all_assets)
        for tbl in tables
    ]

    # Step 3: lower concurrency to serialize upload calls
    updated_tables = {}
    with ThreadPoolExecutor(max_workers=1) as executor:
        futures = {executor.submit(process_table, args): args[0] for args in pool_args}
        for future in as_completed(futures):
            table, updated_data = future.result()
            updated_tables[table] = updated_data

    # Write and upload manifest
    manifest_file = "manifest.json"
    with open(manifest_file, "w") as f:
        json.dump(updated_tables, f, indent=2)

    manifest_url = upload_release_asset(
        github_token, repo_name, release_tag,
        manifest_file, existing_assets=all_assets
    )
    print(f"Uploaded manifest.json to release: {manifest_url}")

    os.remove(manifest_file)
