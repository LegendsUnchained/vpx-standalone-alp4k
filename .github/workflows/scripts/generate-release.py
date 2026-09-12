#!/usr/bin/env python3
import collections
from datetime import datetime, timezone
import os
from urllib.parse import quote
import sys
import time
import json
import hashlib
import shutil
import tempfile
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests
import vpsdb
import catalog_history
import git
from github import Github, Auth
from github.GithubException import GithubException
from pathlib import Path


def md5sum(file_path, chunk_size=1024 * 1024):
    """Return the lowercase MD5 digest of a release asset on disk."""
    digest = hashlib.md5()
    with open(file_path, "rb") as asset:
        for chunk in iter(lambda: asset.read(chunk_size), b""):
            digest.update(chunk)
    return digest.hexdigest()


def find_release(repo, tag, prefer_draft=True):
    """Find a release by tag name, including drafts.

    `repo.get_release(tag)` resolves through /releases/tags/{tag}, which cannot
    see a draft because a draft has no git tag yet, and would silently return
    the previous published release when a candidate reuses its tag. Scanning the
    list is the only way to reach a draft, and lets the draft take precedence.
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
    # A draft wins when both exist. The pipeline builds a candidate as a draft
    # that reuses the final tag, so while it runs there can be a draft and the
    # previous published release sharing one tag — and uploading the new assets
    # onto the published one would corrupt the release currently being served.
    return (draft or published) if prefer_draft else (published or draft)


def find_table_yml(base_dir="tables"):
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


# Files that only affect how a table is presented, never how it plays or
# installs. Excluded from the history fingerprint so a lossless image
# recompression or a README rewrite does not announce ~300 tables as "updated"
# in the Wizard's Recently Updated feed. They still change configVersion, so
# devices do re-download the bundle — the two signals are deliberately separate.
PRESENTATION_FILES = frozenset({
    "README.md",
    "launcher.png",
    "backglass.png",
    "dmd.png",
    "dmdframe.png",
    "playfield.png",
})


def get_config_fingerprint(repo_path, folder_path):
    """
    Content hash of a table's *functional* files only.

    Same content-addressed property as get_config_tree_hash (blob ids, so it
    survives a history rewrite), but blind to presentation. This is what the
    catalog history fingerprints, so "updated" means the table actually
    changed: table.yml, table.ini, table.vbs, VPReg.ini, pinmame/*, and so on.

    Reads the tree, never the blobs, so it costs nothing under the release
    runner's filter=blob:none clone.

    Returns:
        str: A sha256 over the functional (path, blob id) pairs, or None.
    """
    try:
        repo = git.Repo(repo_path)
        listing = repo.git.ls_tree("-r", f"HEAD:{folder_path}")
    except git.GitCommandError:
        print(f"Error: {folder_path} is not present in the checked-out commit")
        return None
    except git.InvalidGitRepositoryError:
        print(f"Error: Invalid Git repository at {repo_path}")
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        return None

    entries = []
    for line in listing.splitlines():
        if not line.strip():
            continue
        meta, path = line.split("\t", 1)
        blob = meta.split()[2]
        if os.path.basename(path) in PRESENTATION_FILES:
            continue
        entries.append(f"{path}\0{blob}")
    digest = hashlib.sha256("\n".join(sorted(entries)).encode()).hexdigest()
    return digest


def get_config_tree_hash(repo_path, folder_path):
    """
    Returns the git tree object id of a table's folder — a hash of its contents.

    This is deliberately content-derived rather than history-derived. A tree id
    depends only on what is in the folder, so it survives a history rewrite:
    flattening the repository changes every commit sha, but a table whose files
    did not change keeps the same tree id. The previous implementation used the
    folder's most recent commit sha, which meant every flatten reassigned all
    ~300 configVersions at once, rebuilt every zip, and advanced every table's
    updatedAt in the catalog history even though no table had actually changed.

    It is also cheaper than the commit walk it replaces: an O(1) tree lookup
    instead of scanning history per folder, and it resolves under the release
    runner's blobless clone because filter=blob:none omits blobs, not trees.

    Args:
        repo_path (str): The path to the local Git repository.
        folder_path (str): The folder path within the repository.

    Returns:
        str: The tree object id, or None if the folder is not in the commit.
    """
    try:
        repo = git.Repo(repo_path)
        # `rev-parse HEAD:<path>` resolves the path to its tree id in the
        # checked-out commit. Preferred over Tree.__getitem__ so the behavior
        # is exactly git's own path resolution.
        return repo.git.rev_parse(f"HEAD:{folder_path}")
    except git.GitCommandError:
        print(f"Error: {folder_path} is not present in the checked-out commit")
        return None
    except git.InvalidGitRepositoryError:
        print(f"Error: Invalid Git repository at {repo_path}")
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        return None


# The tag assets will be published under. Set once in main, because the URL a
# client is given has to be the one that works AFTER the release is published,
# and while the build runs the release is still a draft.
_PUBLISH_TAG = ""


def published_asset_url(repo_name, file_name):
    """The download URL an asset will have once the release is published.

    Deliberately constructed rather than read from asset.browser_download_url.
    A draft has no tag, so GitHub reports its assets under a placeholder
    ("untagged-<hash>"), and that URL dies the moment the release is published.
    Assets are uploaded to a draft here, so every URL recorded from the API was
    a placeholder -- the manifest shipped 404s for exactly the tables the build
    had just rebuilt.
    """
    return (
        f"https://github.com/{repo_name}/releases/download/"
        f"{quote(_PUBLISH_TAG)}/{quote(file_name)}"
    )


def find_inheritance_source(repo, release_tag):
    """The release an incremental build may leave assets in.

    Incremental builds only work if the release being referenced outlives the
    one referencing it, so this is the newest PUBLISHED STABLE release that is
    not the tag being built. Two exclusions, both load-bearing:

    * Prereleases. A candidate is retired -- deleted, with its assets -- as soon
      as the next one is cut, so anything pointing at it starts 404ing then.
    * The tag being built. A candidate can be rebuilt under a tag that already
      exists, and inheriting from that is inheriting from the release about to
      be replaced. That is the shape the manifest was in: 296 tables pointing at
      v2.0.11 while v2.0.11 held 19 zips, every one of them a 404.

    Anchoring on stable also means a table changed during a prerelease chain is
    re-uploaded to each candidate rather than chained across them, so no
    candidate depends on an earlier candidate surviving.
    """
    newest = None
    for rel in repo.get_releases():
        if rel.draft or rel.prerelease:
            continue
        if rel.tag_name == release_tag:
            continue
        stamped = rel.published_at or rel.created_at
        if newest is None or (stamped and stamped > (newest.published_at or newest.created_at)):
            newest = rel
    return newest


def fetch_inheritable_manifest(github_token, repo_name, source):
    """Manifest and asset names of the release incremental builds inherit from.

    Returns (manifest, asset_names, tag). Empty when there is no usable source,
    which makes the build a full rebuild -- the correct fallback, and what every
    pre-flatten release did.
    """
    if source is None:
        return {}, set(), ""
    try:
        names = {a.name for a in source.get_assets()}
        if "manifest.json" not in names:
            return {}, set(), ""
        url = (
            f"https://github.com/{repo_name}/releases/download/"
            f"{quote(source.tag_name)}/manifest.json"
        )
        headers = {
            "Authorization": f"token {github_token}",
            "Accept": "application/octet-stream",
        }
        r = requests.get(url, headers=headers, timeout=30)
        r.raise_for_status()
        print(f"[INFO] Inheriting unchanged assets from {source.tag_name} "
              f"({len(names)} assets)")
        return json.loads(r.text), names, source.tag_name
    except Exception as e:
        print(f"[INFO] No inheritable manifest ({e}); every table will be rebuilt")
        return {}, set(), ""

def build_asset_index(release):
    """
    Build a one-time index of existing assets to avoid repeated pagination.
    """
    by_name = {}
    url_by_name = {}
    for asset in release.get_assets():
        by_name[asset.name] = asset
        url_by_name[asset.name] = published_asset_url(repo_name, asset.name)
    return {"by_name": by_name, "url_by_name": url_by_name}


def upload_release_asset(github_token, repo_name, release, asset_index, index_lock, file_path, clobber=True, max_attempts=3):
    """
    Uploads a file as a release asset using PyGithub. Returns its published
    download URL (see published_asset_url) or None.
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
                    asset_index["url_by_name"][file_name] = published_asset_url(repo_name, file_name)
                return published_asset_url(repo_name, file_name)
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
    table_path = os.path.join("tables", table)
    result = table, table_data.copy()
    if not os.path.isdir(table_path):
        print(f"Warning: Directory {table_path} does not exist, skipping {table}.")
        return result

    # Content hash of this folder (see get_config_tree_hash: tree id, not commit)
    config_version = get_config_tree_hash(".", table_path)
    if not config_version:
        print(f"Error: No config tree found for {table_path}, skipping {table}.")
        return result
    config_fingerprint = get_config_fingerprint(".", table_path)

    # Zip the table directory in a temp folder
    with tempfile.TemporaryDirectory() as tmpdir:
        zip_base = os.path.join(tmpdir, table)
        try:
            shutil.make_archive(zip_base, "zip", table_path)
            zip_path = zip_base + ".zip"
            repo_config_checksum = md5sum(zip_path)
            print(f"Uploading {zip_path} to GitHub...")
            download_url = upload_release_asset(
                github_token, repo_name, release, asset_index, index_lock, zip_path
            )
            if download_url:
                new_data = result[1]
                new_data["repoConfig"] = download_url
                # Hash the exact bytes passed to upload_release_asset so clients
                # can detect truncated or otherwise corrupted downloads.
                new_data["repoConfigChecksum"] = repo_config_checksum
                new_data["configVersion"] = config_version[:7]
                if config_fingerprint:
                    new_data["configFingerprint"] = config_fingerprint[:16]
                new_data["name"] = vpsdb.process_title(new_data["name"], new_data["manufacturer"], new_data["year"])
                result = (table, new_data)
            else:
                print(f"Failed to upload asset for {table}")
        except Exception as e:
            print(f"Error processing {table}: {e}")
    return result


def main():
    github_token = os.environ.get("GITHUB_TOKEN")
    repo_name = os.environ.get("GITHUB_REPOSITORY")
    # RELEASE_TAG, not GITHUB_REF_NAME: the latter is reserved and the runner
    # overrides it with the ref the workflow ran on.
    release_tag = os.environ.get("RELEASE_TAG")

    if not github_token or not repo_name or not release_tag:
        print("Error: Required environment variables not set.")
        sys.exit(1)

    # Every asset URL written into the manifest is built from this, so it has to
    # be set before the first upload.
    global _PUBLISH_TAG
    _PUBLISH_TAG = release_tag

    # Sanity probe: ensure we can reach the release
    try:
        g = Github(auth=Auth.Token(github_token))
        repo = g.get_repo(repo_name)
        rel = find_release(repo, release_tag)
        if rel is None:
            print(f"[ERROR] Release '{release_tag}' not found in '{repo_name}'.")
            sys.exit(1)

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
    # Disabled tables are now excluded inside get_table_meta (before VPSDB
    # resolution), so no post-filter is needed here.
    tables = vpsdb.get_table_meta(files)

    # What this build may inherit instead of rebuilding: the newest stable
    # release that is not this tag. Assets stay where they are and the manifest
    # points back at them.
    source = find_inheritance_source(repo, release_tag)
    prev_manifest, source_assets, source_tag = fetch_inheritable_manifest(
        github_token, repo_name, source
    )

    # Decide which tables changed by comparing the folder's content hash with
    # the manifest's configVersion. Content-derived, so a history rewrite that
    # leaves a table's files alone does not mark it as changed.
    unchanged_tables = []
    reasons = collections.Counter()
    for table, data in list(tables.items()):
        table_path = os.path.join("tables", table)
        latest = get_config_tree_hash(".", table_path)
        short = (latest or "")[:7]
        prev = (prev_manifest.get(table, {}) or {}) if isinstance(prev_manifest, dict) else {}
        prev_short = prev.get("configVersion", "")
        prev_checksum = prev.get("repoConfigChecksum", "")

        if not short or short != prev_short:
            reasons["content changed or new"] += 1
            continue
        # A checksum-less entry is deliberately rebuilt even when its config is
        # unchanged. This backfills manifests published before config bundle
        # verification was introduced.
        if not prev_checksum:
            reasons["no checksum to inherit"] += 1
            continue
        # The inherited URL has to name an asset that exists in the source
        # release, and name the SOURCE -- not this tag, and not some older
        # release that may since have been retired. Without this check the
        # manifest happily carries a URL to something deleted, and the failure
        # only shows up as a 404 on a device mid-install.
        prev_url = prev.get("repoConfig") or ""
        expected = f"/releases/download/{quote(source_tag)}/" if source_tag else None
        asset_name = f"{table}.zip"
        if not expected or expected not in prev_url:
            reasons["inherited URL is not the source release"] += 1
            continue
        if asset_name not in source_assets:
            reasons["asset missing from the source release"] += 1
            continue
        unchanged_tables.append(table)

    if reasons:
        print("[INFO] Rebuilding: "
              + ", ".join(f"{n} {why}" for why, n in reasons.most_common()))

    if unchanged_tables:
        print(f"[INFO] Inheriting {len(unchanged_tables)} unchanged table(s) from "
              f"{source_tag}; their assets are not re-uploaded.")

    # Build asset index once to avoid pagination per file
    asset_index = build_asset_index(rel)
    index_lock = threading.Lock()

    # Only what changed is rebuilt; everything else keeps the URL of the asset
    # already published in the inheritance source (see find_inheritance_source).
    pool_args = [
        (table, tables[table], github_token, repo_name, rel, asset_index, index_lock)
        for table in tables
        if table not in unchanged_tables
    ]

    # Process tables in parallel (adjust max_workers as needed)
    updated_tables = {}
    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = {executor.submit(process_table, arg): arg[0] for arg in pool_args}
        for future in as_completed(futures):
            table, updated_data = future.result()
            if not all(updated_data.get(field) for field in ("repoConfig", "repoConfigChecksum", "configVersion")):
                raise RuntimeError(f"Incomplete config bundle for {table}; refusing to publish discovery history")
            updated_tables[table] = updated_data

    # Unchanged tables keep the entry they had in the source release, URL and
    # all, which is the whole point: the asset stays where it is and this
    # manifest points back at it. Every check above has already established that
    # the URL names an asset the source still holds.
    merged_manifest = {key: dict(prev_manifest[key]) for key in unchanged_tables}
    merged_manifest.update(updated_tables)

    # Stamp every entry, including unchanged ones and no-op reruns. First
    # availability is immutable; content changes get the release's update date.
    history = catalog_history.release_history(repo, rel, merged_manifest)
    catalog_history.stamp(merged_manifest, history)
    # Generated history lives in release assets, never in the checkout.
    with tempfile.TemporaryDirectory(prefix="catalog-history-") as output_dir:
        history_file = os.path.join(output_dir, "table-history.json")
        with open(history_file, "w") as f:
            json.dump(history, f, indent=2, sort_keys=True)
        history_url = upload_release_asset(github_token, repo_name, rel, asset_index, index_lock, history_file, clobber=True)
        if not history_url:
            raise RuntimeError("Could not publish table history; refusing to publish an incomplete catalog")

    # Write & upload manifest
    manifest_file = "manifest.json"
    with open(manifest_file, "w") as f:
        json.dump(merged_manifest, f, indent=2)

    manifest_url = upload_release_asset(github_token, repo_name, rel, asset_index, index_lock, manifest_file, clobber=True)
    if not manifest_url:
        raise RuntimeError("Could not publish manifest.json")
    print(f"Uploaded manifest.json to release: {manifest_url}")

    # A small, stable summary of the release, so a client that only wants a few
    # facts does not have to pull the whole manifest for them.
    #
    # The table count in particular cannot be derived from the release any more.
    # It used to be read by counting vpx-*.zip assets, which worked only while
    # every release carried every zip; an incremental release carries the
    # changed ones, so that count became "tables changed in this release" and
    # the repo picker started reporting a 315-table catalog as 19.
    #
    # manifest.json is ~1.2 MiB, and the picker reads one per offered repo, so
    # the difference is a listing that is instant rather than one that downloads
    # several megabytes to render two numbers per row.
    meta_file = "release-meta.json"
    release_meta = {
        # Bump when a field changes meaning. Readers should tolerate unknown
        # fields and a missing file: releases published before this exists, and
        # forks that have not rebuilt, simply do not have one.
        "schemaVersion": 1,
        "repo": repo_name,
        "version": release_tag,
        # When the release was BUILT. Not when it was published: at this point
        # it is still a draft and has no publish date. Clients wanting that have
        # it already, from the release API response that led them here.
        "generatedAt": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "tableCount": len(merged_manifest),
        "newCount": sum(
            1 for v in merged_manifest.values()
            if v.get("firstAvailableRelease") == release_tag
        ),
        "updatedCount": sum(
            1 for v in merged_manifest.values()
            if v.get("updatedRelease") == release_tag
        ),
        # Lets a client decide whether the manifest it already has is still
        # current without downloading it again.
        "manifestBytes": os.path.getsize(manifest_file),
        "manifestChecksum": md5sum(manifest_file),
    }
    with open(meta_file, "w") as f:
        json.dump(release_meta, f, indent=2, sort_keys=True)
    meta_url = upload_release_asset(
        github_token, repo_name, rel, asset_index, index_lock, meta_file, clobber=True
    )
    if not meta_url:
        raise RuntimeError("Could not publish release-meta.json")
    print(f"Uploaded release-meta.json: {release_meta['tableCount']} tables, "
          f"{release_meta['newCount']} new, {release_meta['updatedCount']} updated")
    # Left in the workspace on purpose. Later steps in this job read it from
    # disk rather than downloading it back from the release, which is both a
    # pointless round trip and the thing that kept breaking: a draft release
    # has no publicly fetchable asset URL.


if __name__ == "__main__":
    main()
