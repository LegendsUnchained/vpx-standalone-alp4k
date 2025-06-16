import os
import re
import shutil
import json

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
        commits = list(repo.iter_commits(paths=folder_path, max_count=None)) # max_count = None to get all commits.

        if commits:
            for commit in commits:
                files = commit.stats.files
                for file in files:
                    if file.startswith(folder_path):
                        return commit.hexsha
            return None
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

    files = find_table_yml()

    tables = vpsdb.get_table_meta(files)
    
    tables_to_remove = []
    for table, table_data in tables.items():
        if table_data.get("enabled") is False:
            print(f"Skipping disabled table: {table}")
            tables_to_remove.append(table)
            continue

        external_path = os.path.join("external", table)

        print(f"Zipping {table} for release")
        shutil.make_archive(table, "zip", external_path)

        print(f"Uploading {table}.zip to GitHub")
        file_path = f"{table}.zip"
        download_url = upload_release_asset(
            github_token, repo_name, release_tag, file_path
        )

        print(f"Cleaning up {table}.zip")
        os.remove(file_path)

        tables[table]["repoConfig"] = download_url

        config_version = get_latest_commit_hash(".", external_path)
        if config_version:
            tables[table]["configVersion"] = config_version[:7]
        else:
            print(f"Error: No commit found for {external_path}")
            exit(1)

        # Apply field processing
        tables[table]["name"] = process_title(tables[table]["name"], tables[table]["manufacturer"], tables[table]["year"])

    for table in tables_to_remove:
        del tables[table]

    manifest_file = "manifest.json"
    json.dump(tables, open(manifest_file, "w"))
    upload_release_asset(github_token, repo_name, release_tag, manifest_file)
