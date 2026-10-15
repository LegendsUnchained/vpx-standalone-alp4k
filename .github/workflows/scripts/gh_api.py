"""Just enough of the GitHub REST API for the release scripts (stdlib only).

Shared by prerelease.py (the rolling pre-release sync) and promotion.py
(promoting tables to stable), so neither needs PyGithub for the parts that run
on every push or every bot dry run.
"""
import json
import os
import socket
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

API = os.environ.get("GITHUB_API_URL", "https://api.github.com")
UPLOADS = "https://uploads.github.com"

# Reads and uploads are retried after a GitHub 5xx or a dropped connection;
# these are the waits before each retry. A promotion makes dozens of asset
# downloads and uploads, and one transient 500 or 502 used to fail the run.
RETRY_WAITS = (5, 15, 45)


def _transient(e):
    """A failure worth retrying: a 5xx, or no answer at all."""
    if isinstance(e, urllib.error.HTTPError):
        return e.code >= 500
    return isinstance(e, (urllib.error.URLError, socket.timeout, ConnectionError))


def _retrying(what, fn, before_retry=None):
    """fn(), retried after a transient failure. before_retry runs before each
    retry, to undo what a failed attempt may have left behind."""
    for attempt, wait in enumerate((*RETRY_WAITS, None)):
        try:
            return fn()
        except Exception as e:
            if wait is None or not _transient(e):
                raise
            print(f"{what} failed ({e}); retry {attempt + 1} of {len(RETRY_WAITS)} in {wait}s",
                  file=sys.stderr)
        time.sleep(wait)
        if before_retry:
            before_retry()


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


class Api:
    def __init__(self, repo, token):
        self.repo = repo
        self.token = token
        self._plain = urllib.request.build_opener(_NoRedirect)

    @classmethod
    def from_env(cls):
        repo = os.environ.get("GITHUB_REPOSITORY")
        if not repo:
            sys.exit("GITHUB_REPOSITORY is not set")
        return cls(repo, os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN"))

    def _request(self, method, url, body=None, content_type="application/json",
                 accept="application/vnd.github+json"):
        headers = {"Accept": accept, "X-GitHub-Api-Version": "2022-11-28"}
        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"
        data = None
        if body is not None:
            data = body if isinstance(body, bytes) else json.dumps(body).encode()
            headers["Content-Type"] = content_type
        req = urllib.request.Request(url, data=data, method=method, headers=headers)
        try:
            return self._plain.open(req, timeout=120)
        except urllib.error.HTTPError as e:
            if e.code in (301, 302, 303, 307, 308):
                return e
            # GitHub's message says why (rate limit, permissions, validation);
            # the status line alone does not.
            detail = e.read().decode("utf-8", "replace")[:500]
            print(f"{method} {url} -> {e.code}: {detail}", file=sys.stderr)
            raise

    def call(self, method, path, body=None):
        url = path if path.startswith("http") else f"{API}/repos/{self.repo}/{path}"

        def once():
            with self._request(method, url, body) as resp:
                return resp.read()
        # Only reads are retried: a write that failed may still have happened.
        raw = _retrying(f"{method} {url}", once) if method == "GET" else once()
        return json.loads(raw) if raw else None

    def get(self, path):
        return self.call("GET", path)

    def get_or_none(self, path):
        try:
            return self.get(path)
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return None
            raise

    def exists(self, path):
        return self.get_or_none(path) is not None

    def paginate(self, path):
        sep = "&" if "?" in path else "?"
        page, out = 1, []
        while True:
            batch = self.get(f"{path}{sep}per_page=100&page={page}")
            out.extend(batch)
            if len(batch) < 100:
                return out
            page += 1

    def release_by_tag(self, tag):
        """A published release by tag, or None. Drafts are invisible here."""
        return self.get_or_none(f"releases/tags/{urllib.parse.quote(tag)}")

    def assets(self, release_id):
        return self.paginate(f"releases/{release_id}/assets")

    def asset_bytes(self, asset_id):
        """An asset's bytes, drafts included.

        The API redirects to a pre-signed storage URL. The redirect is followed
        by hand so the token is not sent along to storage, which rejects a
        request that carries two kinds of credentials.
        """
        url = f"{API}/repos/{self.repo}/releases/assets/{asset_id}"

        def once():
            resp = self._request("GET", url, accept="application/octet-stream")
            if resp.status in (301, 302, 303, 307, 308):
                location = resp.headers["Location"]
                resp.close()
                with urllib.request.urlopen(location, timeout=300) as follow:
                    return follow.read()
            with resp:
                return resp.read()
        return _retrying(f"Downloading asset {asset_id}", once)

    def named_bytes(self, assets, name):
        """Bytes of the asset called name in an asset list, or None."""
        asset = next((a for a in assets if a["name"] == name), None)
        return self.asset_bytes(asset["id"]) if asset else None

    def upload(self, release_id, name, data, content_type="application/octet-stream"):
        """Upload an asset, retrying a GitHub 5xx or a dropped connection.

        A failed upload can still leave an asset of that name behind (often
        in the "starter" state), and a second upload of the name then fails
        with 422 already_exists. So each retry first deletes any asset the
        release already has under the name.
        """
        url = (f"{UPLOADS}/repos/{self.repo}/releases/{release_id}/assets"
               f"?name={urllib.parse.quote(name)}")

        def once():
            with self._request("POST", url, data, content_type=content_type) as resp:
                return json.loads(resp.read())

        def clear_half_upload():
            for a in self.assets(release_id):
                if a["name"] == name:
                    print(f"  deleting the half-uploaded {name} (state {a.get('state')})", file=sys.stderr)
                    self.delete_asset(a["id"])
        return _retrying(f"Uploading {name}", once, clear_half_upload)

    def delete_asset(self, asset_id):
        self.call("DELETE", f"releases/assets/{asset_id}")

    def replace_asset(self, release_id, assets, name, data, content_type="application/json"):
        """Replace an asset by name with as short a gap as the API allows.

        Assets cannot be overwritten, only deleted and re-uploaded, and the
        upload is the slow part. So the new bytes go up under a temporary name
        first; the old asset is then deleted and the new one renamed, leaving a
        gap of a single API call in which the name does not resolve.
        """
        staged = f"{name}.next"
        for a in assets:
            if a["name"] == staged:  # left over from a failed run
                self.delete_asset(a["id"])
        new = self.upload(release_id, staged, data, content_type)
        for a in assets:
            if a["name"] == name:
                self.delete_asset(a["id"])
        self.call("PATCH", f"releases/assets/{new['id']}", {"name": name})


def asset_url(repo, tag, name):
    """The public download URL of a release asset."""
    return (f"https://github.com/{repo}/releases/download/"
            f"{urllib.parse.quote(tag)}/{urllib.parse.quote(name)}")


def asset_name(url):
    """The asset file name a download URL points at."""
    return urllib.parse.unquote((url or "").rsplit("/", 1)[-1])
