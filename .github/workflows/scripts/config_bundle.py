"""A table folder's config bundle: its content ids and its zip (stdlib only).

Two content-derived signals, deliberately separate (see catalog-history.md):

* configVersion: the folder's git tree id. Any change to the folder changes it,
  so devices re-download the bundle, new launcher.png included.
* configFingerprint: a hash over the folder's functional files only, so
  catalog history calls a table "updated" only when it actually changed.

Both survive a history rewrite: they depend on file content, not commits.
"""
import hashlib
import os
import shutil
import subprocess

# Files that only affect how a table is presented, never how it plays or
# installs. Excluded from the fingerprint so a lossless image recompression or a
# README rewrite does not announce tables as "updated".
PRESENTATION_FILES = frozenset({
    "README.md",
    "launcher.png",
    "backglass.png",
    "dmd.png",
    "dmdframe.png",
    "playfield.png",
})


def _git(*args, cwd="."):
    return subprocess.run(["git", *args], cwd=cwd, capture_output=True, text=True,
                          check=True).stdout


def folder_trees(rev="HEAD", base="tables", cwd="."):
    """{folder name: tree id} for every folder directly under base at rev."""
    out = {}
    for line in _git("ls-tree", rev, f"{base}/", cwd=cwd).splitlines():
        meta, path = line.split("\t", 1)
        kind, sha = meta.split()[1:3]
        if kind == "tree":
            out[path.rsplit("/", 1)[-1]] = sha
    return out


def fingerprint(folder, rev="HEAD", cwd="."):
    """sha256 over the (path, blob id) pairs of the folder's functional files."""
    entries = []
    for line in _git("ls-tree", "-r", f"{rev}:{folder}", cwd=cwd).splitlines():
        meta, path = line.split("\t", 1)
        if os.path.basename(path) in PRESENTATION_FILES:
            continue
        entries.append(f"{path}\0{meta.split()[2]}")
    return hashlib.sha256("\n".join(sorted(entries)).encode()).hexdigest()


def md5sum(path, chunk_size=1024 * 1024):
    digest = hashlib.md5()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(chunk_size), b""):
            digest.update(chunk)
    return digest.hexdigest()


def build_zip(folder, out_base):
    """Zip folder's contents (as the release build does). Returns (path, md5)."""
    path = shutil.make_archive(out_base, "zip", folder)
    return path, md5sum(path)
