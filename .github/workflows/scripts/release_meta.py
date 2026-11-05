"""release-meta.json: a small, stable summary of a release (stdlib only).

Shared by generate-release.py (fork releases), prerelease.py (the rolling
pre-release) and promotion.py (stable releases), so all three publish exactly
the same shape.

A client that only wants a few facts should not have to pull the whole manifest
for them. The table count in particular cannot be derived from the release any
more. It used to be read by counting vpx-*.zip assets, which worked only while
every release carried every zip; an incremental release carries the changed
ones, so that count became "tables changed in this release" and the repo picker
started reporting a 315-table catalog as 19.

manifest.json is ~1.2 MiB, and the picker reads one per offered repo, so the
difference is a listing that is instant rather than one that downloads several
megabytes to render two numbers per row.
"""
from datetime import datetime, timezone
import hashlib
import os


def md5sum(file_path, chunk_size=1024 * 1024):
    """Return the lowercase MD5 digest of a file on disk."""
    digest = hashlib.md5()
    with open(file_path, "rb") as fh:
        for chunk in iter(lambda: fh.read(chunk_size), b""):
            digest.update(chunk)
    return digest.hexdigest()


def build(repo_name, release_tag, manifest, manifest_file):
    """The release-meta.json payload for a stamped manifest written to disk."""
    return {
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
        "tableCount": len(manifest),
        "newCount": sum(
            1 for v in manifest.values()
            if v.get("firstAvailableRelease") == release_tag
        ),
        "updatedCount": sum(
            1 for v in manifest.values()
            if v.get("updatedRelease") == release_tag
        ),
        # Lets a client decide whether the manifest it already has is still
        # current without downloading it again.
        "manifestBytes": os.path.getsize(manifest_file),
        "manifestChecksum": md5sum(manifest_file),
    }
