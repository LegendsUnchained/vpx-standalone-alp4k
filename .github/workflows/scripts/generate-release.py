import os
import re
import shutil
import json
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed

import vpsdb
import git
from github import Github
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

def upload_release_asset(github_token, repo_name, release_tag, file_path, clobber=True):
    # Uploads a file as a release asset.
    try:
        g = Github(github_token)
        repo = g.get_repo(repo_name)
        release = repo.get_release(release_tag)
        file_name = os.path.basename(file_path)
        # Check if the asset already exists and clobber if needed
        existing_assets = list(release.get_assets())
        for asset in existing_assets:
            if asset.name == file_name and clobber:
                asset.delete_asset()
                break
        asset = release.upload_asset(file_path, label=file_name)
        print(f"Uploaded {file_name} to release {release_tag}")
        # Refresh the release object and return download URL
        release = repo.get_release(release_tag)
        for asset in release.get_assets():
            if asset.name == file_name:
                return asset.browser_download_url
    except Exception as e:
        print(f"Error uploading asset: {e}")
        return None

def process_table(args):
    # Unpack for ThreadPoolExecutor compatibility
    table, table_data, github_token, repo_name, release_tag = args
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

    # Optional: Skip if unchanged (requires storing previous version info in manifest)
    # Uncomment below if you want to skip tables whose hash matches what's already in manifest:
    # if table_data.get("configVersion") == config_version[:7]:
    #     print(f"Skipping unchanged table: {table}")
    #     return result

    # Zip the table directory in a temp folder
    with tempfile.TemporaryDirectory() as tmpdir:
        zip_base = os.path.join(tmpdir, table)
        try:
            shutil.make_archive(zip_base, "zip", external_path)
            zip_path = zip_base + ".zip"
            print(f"Uploading {zip_path} to GitHub...")
            download_url = upload_release_asset(
                github_token, repo_name, release_tag, zip_path
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

if __name__ == "__main__":
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name = os.environ.get("GITHUB_REPOSITORY")
    release_tag = os.environ.get("GITHUB_REF_NAME")
    if not github_token or not repo_name or not release_tag:
        print("Error: Required environment variables not set.")
        exit(1)

    files = find_table_yml()
    tables = vpsdb.get_table_meta(files)

    # Remove disabled tables before processing
    tables_to_remove = [table for table, data in tables.items() if data.get("enabled") is False]
    for table in tables_to_remove:
        print(f"Skipping disabled table: {table}")
        del tables[table]

    # Prepare arguments for parallel processing
    pool_args = [
        (table, tables[table], github_token, repo_name, release_tag)
        for table in tables
    ]

    # Process tables in parallel (adjust max_workers as needed)
    updated_tables = {}
    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = {executor.submit(process_table, arg): arg[0] for arg in pool_args}
        for future in as_completed(futures):
            table, updated_data = future.result()
            updated_tables[table] = updated_data

    # Write updated manifest
    manifest_file = "manifest.json"
    with open(manifest_file, "w") as f:
        json.dump(updated_tables, f, indent=2)

    # Upload manifest.json to release
    manifest_url = upload_release_asset(github_token, repo_name, release_tag, manifest_file)
    print(f"Uploaded manifest.json to release: {manifest_url}")

    # Optional: remove manifest after upload
    os.remove(manifest_file)
