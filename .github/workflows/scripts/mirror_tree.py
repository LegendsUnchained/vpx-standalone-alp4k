"""Edit a catalog mirror branch's tree with git plumbing (stdlib only).

The `manifest` and `manifest-testing` branches are CORS mirrors of the catalog
data (see publish-catalog-data.yml). Each write is one fresh orphan commit.
Building it from an existing tree, rather than from files on disk, keeps every
blob that did not change exactly as it was: box art testers were served is the
art stable gets, and a push carries only the few files that did change.
"""
import os
import subprocess
import tempfile


def git(*args, env=None, input=None, check=True):
    result = subprocess.run(["git", *args], capture_output=True, env=env,
                            input=input if input is None or isinstance(input, bytes)
                            else input.encode())
    if check and result.returncode:
        raise RuntimeError(f"git {' '.join(args)}: {result.stderr.decode().strip()}")
    return result.stdout.decode().strip()


def show(ref, path):
    """A file's bytes at ref, or None if it is not there."""
    result = subprocess.run(["git", "show", f"{ref}:{path}"], capture_output=True)
    return result.stdout if result.returncode == 0 else None


def tree_entries(ref, path):
    """[(mode, sha, path)] of the blobs under path in ref, recursively."""
    rows = []
    for line in git("ls-tree", "-r", ref, "--", path).splitlines():
        meta, p = line.split("\t", 1)
        mode, _, sha = meta.split()
        rows.append((mode, sha, p))
    return rows


def remote_url(repo, token):
    return f"https://x-access-token:{token}@github.com/{repo}.git"


def fetch_branches(repo, token, *branches):
    """Fetch the named mirror branches as origin/<name>; missing ones are skipped."""
    fetched = []
    for branch in branches:
        result = subprocess.run(
            ["git", "fetch", "-q", "--depth", "1", remote_url(repo, token),
             f"+refs/heads/{branch}:refs/remotes/origin/{branch}"],
            capture_output=True)
        if result.returncode == 0:
            fetched.append(branch)
    return fetched


def push(repo, token, commit, branch):
    """Force the branch to commit. Returns the sha it pointed at before, or ''."""
    before = git("rev-parse", "-q", "--verify", f"refs/remotes/origin/{branch}", check=False)
    git("push", "-q", "--force", remote_url(repo, token), f"{commit}:refs/heads/{branch}")
    return before


class TreeBuilder:
    """A scratch index seeded from a ref's tree."""

    def __init__(self, base_ref=None):
        self._tmp = tempfile.TemporaryDirectory()
        self.env = dict(os.environ, GIT_INDEX_FILE=os.path.join(self._tmp.name, "index"))
        if base_ref:
            git("read-tree", f"{base_ref}^{{tree}}", env=self.env)
        else:
            git("read-tree", "--empty", env=self.env)

    def put(self, path, data):
        sha = git("hash-object", "-w", "--stdin", input=data)
        git("update-index", "--add", "--cacheinfo", f"100644,{sha},{path}", env=self.env)

    def drop(self, path):
        git("rm", "-r", "--cached", "-q", "--ignore-unmatch", "--", path, env=self.env)

    def take(self, ref, path):
        """Copy path (file or folder) from ref. Nothing there leaves it as is."""
        rows = tree_entries(ref, path)
        if rows:
            self.drop(path)
        for mode, sha, p in rows:
            git("update-index", "--add", "--cacheinfo", f"{mode},{sha},{p}", env=self.env)
        return bool(rows)

    def put_dir(self, local_dir, prefix):
        """Add every file under local_dir at prefix/<relative path>."""
        for root, _, files in os.walk(local_dir):
            for name in files:
                full = os.path.join(root, name)
                rel = os.path.relpath(full, local_dir).replace(os.sep, "/")
                with open(full, "rb") as fh:
                    self.put(f"{prefix}/{rel}", fh.read())

    def commit(self, message):
        tree = git("write-tree", env=self.env)
        self._tmp.cleanup()
        return git("commit-tree", tree, "-m", message)
