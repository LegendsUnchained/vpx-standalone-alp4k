#!/usr/bin/env python3

import argparse
import json
import os
import requests
import sys



from github import Github


# Each entry links to the table's page in the Table Manager Wizard catalog.
# Same URL the per-table READMEs point at, keyed on the table folder name.
CATALOG_URL = "https://vpxtablemanager.com/catalog/#table={key}"


def find_release(repo, tag):
    """Find a release by tag name.

    Scanned rather than resolved through /releases/tags/{tag}: that endpoint
    cannot see a draft, and returns the previous published release when a
    candidate reuses its tag.
    """
    draft = None
    published = None
    for release in repo.get_releases():
        if release.tag_name != tag:
            continue
        if release.draft:
            draft = draft or release
        else:
            published = published or release
    # Draft first: a candidate reuses the final tag, so while the pipeline runs
    # a draft and the previous published release share one. Writing the notes to
    # the published one would rewrite the release users are being served.
    return draft or published


def get_wizard_data(repo, tag, token):
    """Read the manifest.json asset attached to a release.

    Notes are written while the release is still a draft, so this reads the
    asset through the authenticated API endpoint. A draft has no public
    download URL at all — browser_download_url points at a synthetic
    .../download/untagged-<hash>/ path that 404s regardless of credentials.
    """
    try:
        release = find_release(repo, tag)
        if release is None:
            print(f"Error: no release found with tag {tag}", file=sys.stderr)
            return None
        for asset in release.get_assets():
            if asset.name == "manifest.json":
                # asset.url (the API endpoint), not browser_download_url. A
                # draft's browser URL is a synthetic .../download/untagged-<hash>/
                # path that 404s even with a token; the API endpoint serves the
                # bytes when asked for octet-stream.
                response = requests.get(
                    asset.url,
                    headers={
                        "Authorization": f"token {token}",
                        "Accept": "application/octet-stream",
                    },
                    timeout=30,
                )
                response.raise_for_status()
                return json.loads(response.text)
    except Exception as e:
        print(f"Error getting wizard data for tag {tag}: {e}", file=sys.stderr)
        return None


def classify_from_manifest(manifest, tag):
    """Split a stamped manifest into tables new in this release and tables updated by it.

    The catalog history already answers this exactly: generate-release.py stamps
    every entry with firstAvailableRelease and updatedRelease from the ledger,
    which is derived from release assets and each table's content fingerprint.

    This used to diff the two release tags in git, which broke the moment the
    repository was flattened: every tag was retargeted onto the flatten commit,
    so a tag-to-tag comparison showed no table changes at all and the notes came
    out empty. Reading the manifest is immune to that, and is also more accurate
    — it ignores presentation-only churn like recompressed art, exactly as the
    Recently Updated feed does.
    """
    added, modified = [], []
    for key, entry in sorted(manifest.items()):
        if not isinstance(entry, dict) or entry.get("enabled") is False:
            continue
        if entry.get("firstAvailableRelease") == tag:
            added.append(key)
        elif entry.get("updatedRelease") == tag:
            modified.append(key)
    return added, modified


def get_release_notes(added, modified, wizard_data):
    """Render the added/updated table lists. Format unchanged from the
    git-diff implementation this replaced, so published notes stay consistent."""
    if not added and not modified:
        print("No tables were added or updated in this release.")
        return None

    def entry(key):
        name = (wizard_data or {}).get(key, {}).get("name", key)
        # The folder key stays visible: contributors refer to tables by folder,
        # and it is what the catalog link is keyed on.
        return f"- [{name}]({CATALOG_URL.format(key=key)}) (`{key}`)"

    release_notes = []
    if added:
        release_notes.append("## Newly added tables")
        release_notes.extend(entry(key) for key in added)
    if modified:
        release_notes.append("## Updated tables:")
        release_notes.extend(entry(key) for key in modified)
    return "\n".join(release_notes)


def main():
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name = os.environ.get("GITHUB_REPOSITORY")
    # RELEASE_TAG, not GITHUB_REF_NAME: the latter is reserved and the runner
    # overrides it with the ref the workflow ran on.
    release_tag = os.environ.get("RELEASE_TAG")

    parser = argparse.ArgumentParser(
        description="Write a release's notes from its own published manifest."
    )
    parser.add_argument(
        "--tag", default=release_tag, help="Release tag to write notes for."
    )
    parser.add_argument(
        "--repository", default=repo_name, help="Repository to check (owner/repo)."
    )
    parser.add_argument("--github-token", default=github_token, help="Github token")
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Print the notes instead of writing them to the release.",
    )

    args = parser.parse_args()
    tag = args.tag

    if not args.github_token:
        print("Error: --github-token is required.", file=sys.stderr)
        sys.exit(1)
    if not tag:
        print("Error: --tag is required.", file=sys.stderr)
        sys.exit(1)

    try:
        g = Github(args.github_token)
        repo = g.get_repo(args.repository or os.environ.get("GITHUB_REPOSITORY"))
    except Exception as e:
        print(f"Error connecting to GitHub: {e}", file=sys.stderr)
        sys.exit(1)

    # The manifest this release just published is the source of truth: its
    # entries are stamped with firstAvailableRelease/updatedRelease from the
    # catalog history, so no git comparison is involved and a flattened
    # repository makes no difference.
    manifest = get_wizard_data(repo, tag, args.github_token)
    if not manifest:
        print(f"Error: no manifest.json asset on release '{tag}'.", file=sys.stderr)
        sys.exit(1)

    added, modified = classify_from_manifest(manifest, tag)
    print(f"{len(added)} table(s) new in {tag}, {len(modified)} updated.")
    new_release_notes = get_release_notes(added, modified, manifest)
    if new_release_notes is None:
        sys.exit(0)

    print("Adding release notes to the release...")
    print(new_release_notes)
    if args.dry_run:
        return

    try:
        release = find_release(repo, tag)
        # draft/prerelease must be echoed back: PyGithub defaults both to
        # False, so omitting them would promote the prerelease to a full
        # release just by writing its notes.
        release.update_release(name=release.title, message=new_release_notes,
                               draft=release.draft, prerelease=release.prerelease)
        print(f"Release notes for tag '{tag}' updated successfully.")
    except Exception as e:
        print(f"Error editing release notes: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
