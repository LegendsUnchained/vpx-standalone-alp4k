#!/usr/bin/env python3

import argparse
import json
import os
import requests
import sys

import yaml

from github import Github
from pathlib import Path


def get_commit_hash_from_release(repo, tag):
    """Get commit hash from release tag using the GitHub Releases API."""
    try:
        repo.get_releases()
        release = repo.get_release(tag)
        tags = repo.get_tags()
        for t in tags:
            if t.name == tag:
                return t.commit.sha
    except Exception as e:
        print(f"Error getting commit hash for tag {tag}: {e}", file=sys.stderr)
        return None


def get_wizard_data(repo, tag):
    """Get wizard data from release tag using the GitHub Releases API."""
    try:
        release = repo.get_release(tag)
        assets = release.get_assets()
        for asset in assets:
            if asset.name == "manifest.json":
                # download the asset with requests
                response = requests.get(asset.browser_download_url)
                if response.status_code == 200:
                    return response.json()
    except Exception as e:
        print(f"Error getting wizard data for tag {tag}: {e}", file=sys.stderr)
        return None


def find_changed_files(repo, tag1_hash, tag2_hash):
    """Find changed table.yml files using the GitHub API."""
    try:
        enabled_tables = get_enabled_tables()

        if enabled_tables:
            comparison = repo.compare(tag1_hash, tag2_hash)
            changed_files = []
            for file in comparison.files:
                path = Path(file.filename)
                folder_name = path.parent.name

                if (
                    folder_name not in enabled_tables
                    or path.name.lower() == "readme.md"
                ):
                    continue

                status = "modified"
                if path.name == "table.yml" and file.status == "added":
                    status = "added"

                changed_folder = f"{status}\t{folder_name}"
                if changed_folder not in changed_files:
                    changed_files.append(changed_folder)
            return "\n".join(changed_files)
    except Exception as e:
        print(f"Error finding changed files: {e}", file=sys.stderr)
        return None


def get_enabled_tables():
    """Find all files named table.yml in the 'external' folder relative to the cwd."""
    external_dir = os.path.join(os.getcwd(), "external")
    table_yml_files = []

    if not os.path.exists(external_dir) or not os.path.isdir(external_dir):
        return table_yml_files  # Return empty list if 'external' folder doesn't exist

    for root, _, files in os.walk(external_dir):
        for file in files:
            if file == "table.yml":
                table_yml_files.append(os.path.join(root, file))

    enabled_tables = []
    for table_yml in table_yml_files:
        with open(table_yml, "r") as f:
            y = yaml.safe_load(f)
            if "enabled" in y and not y["enabled"]:
                print(f"Skipping {table_yml} because it is disabled.")
                continue

            path = Path(table_yml)
            folder_name = path.parent.name
            enabled_tables.append(folder_name)
    return enabled_tables


def get_release_notes(changed_files, wizard_data):
    """List added and changed table.yml files."""
    if not changed_files:
        print("No table.yml files have changed or been added since the last release.")
    else:
        added_files = []
        modified_files = []
        for line in changed_files.splitlines():
            parts = line.split("\t")
            if len(parts) >= 2:
                status, file = parts[0], parts[1]
                if status == "added":
                    added_files.append(file)
                elif status == "modified":
                    modified_files.append(file)

        release_notes = []
        if added_files:
            release_notes.append("## Newly added tables")
            for file in added_files:
                table_name = wizard_data.get(file, {}).get("name", file)
                release_notes.append(f"- {table_name} ({file})")
        if modified_files:
            release_notes.append("## Updated tables:")
            for file in modified_files:
                table_name = wizard_data.get(file, {}).get("name", file)
                release_notes.append(f"- {table_name} ({file})")

        return "\n".join(release_notes)


def get_previous_tag(repo, current_tag):
    """Get the tag immediately before the current tag using the GitHub Releases API."""
    try:
        releases = repo.get_releases()
        releases_list = [release.tag_name for release in releases]
    except Exception as e:
        print(f"Error getting releases: {e}", file=sys.stderr)
        return None

    found = False
    for tag in releases_list:
        if found:
            return tag
        if tag == current_tag:
            found = True
    return None


def main():
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name = os.environ.get("GITHUB_REPOSITORY")
    release_tag = os.environ.get("GITHUB_REF_NAME")

    parser = argparse.ArgumentParser(
        description="List changed table.yml files between tags."
    )
    parser.add_argument("--start-tag", help="Start tag for diff.")
    parser.add_argument(
        "--end-tag", default=release_tag, help="End tag for diff. (Required)"
    )
    parser.add_argument(
        "--repository", default=repo_name, help="Repository to check (owner/repo)."
    )
    parser.add_argument(
        "--github-token", default=github_token, help="Github token", required=True
    )

    args = parser.parse_args()

    end_tag = args.end_tag
    start_tag = args.start_tag
    repository = args.repository
    github_token = args.github_token

    if not end_tag:
        print("Error: --end-tag is required.", file=sys.stderr)
        sys.exit(1)

    try:
        g = Github(github_token)
        repo = g.get_repo(repository or os.environ.get("GITHUB_REPOSITORY"))
    except Exception as e:
        print(f"Error connecting to GitHub: {e}", file=sys.stderr)
        sys.exit(1)

    if not start_tag:
        start_tag = get_previous_tag(repo, end_tag)

    if start_tag:
        tag1_hash = get_commit_hash_from_release(repo, start_tag)
        tag2_hash = get_commit_hash_from_release(repo, end_tag)

        if tag1_hash and tag2_hash:
            changed_files = find_changed_files(repo, tag1_hash, tag2_hash)
            wizard_data = get_wizard_data(repo, end_tag)
            new_release_notes = get_release_notes(changed_files, wizard_data)
            print("Adding release notes to the release...")
            print(new_release_notes)
            try:
                release = repo.get_release(end_tag)
                release.update_release(name=release.title, message=new_release_notes)
                print(f"Release notes for tag '{end_tag}' updated successfully.")
            except Exception as e:
                print(f"Error editing release notes: {e}", file=sys.stderr)
        else:
            print("Error: Failed to get commit hashes for tags.", file=sys.stderr)
            sys.exit(1)
    else:
        print("No previous release tag found.")


if __name__ == "__main__":
    main()
