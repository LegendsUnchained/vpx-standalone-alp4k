#!/usr/bin/env python3
"""Keep the rolling pre-release in step with main.

There is one pre-release, tagged `pre-release`, and it is never re-cut: each
sync updates it in place. It carries the DELTA between main and stable, every
Wizard table whose folder on main differs from what stable ships:

    added    on main, not in stable
    updated  in stable, but main's folder tree id is not its configVersion
    removed  in stable, but gone from main or `enabled: false` there

and publishes a full testing manifest.json, stable's with the delta applied, so
Table Manager's testing tracks, the web catalog and Discord all read one file.
delta.json says what is staged; Promote Release reads it.

Only the delta is ever zipped, and a delta table is zipped once per version:
its zip is named by content (vpx-x-<configVersion>.zip), so a later sync finds
it already uploaded. Zips nothing references any more are purged, one sync
late, so a device holding the previous manifest still finds its download.

The delta is decided by folder trees alone. A table whose files are untouched
keeps stable's entry, VPSDB-side metadata included, until its folder changes.

    prerelease.py sync [--dry-run] [--out-dir DIR] [--summary FILE]
"""
import argparse
import copy
import hashlib
import json
import os
import subprocess
import sys
import tempfile
from datetime import datetime, timezone

import catalog_history
import config_bundle
import gh_api
import mirror_tree

TAG = "pre-release"
TABLES = "tables"
# Staged tables are not in the catalog yet: link each to its card on the
# testers page, where it is reviewed and signed off.
TESTERS_URL = "https://vpxtablemanager.com/testers/#table={key}"
EXTRAS = {  # catalog files that are not tables: mirror path -> repo path
    "achievements.json": "tm-config/achievements.json",
    "team_favorites.json": "team_favorites.json",
    "editors_picks.json": "editors_picks.json",
}
SCRIPTS = os.path.dirname(os.path.abspath(__file__))


# --- Pure logic (unit tested) -----------------------------------------------

def zip_name(key, version):
    return f"{key}-{version}.zip"


def compute_delta(stable, trees, enabled):
    """{key: {"change", "configVersion"}} between stable and main.

    trees maps each Wizard folder on main (one with a table.yml) to its tree
    id; enabled is the set of those whose table.yml is not `enabled: false`.
    """
    delta = {}
    for key in sorted(enabled):
        version = trees[key][:7]
        if key not in stable:
            delta[key] = {"change": "added", "configVersion": version}
        elif not trees[key].startswith(stable[key].get("configVersion") or "-"):
            delta[key] = {"change": "updated", "configVersion": version}
    for key in sorted(stable):
        if key not in enabled:
            delta[key] = {"change": "removed", "configVersion": None}
    return dict(sorted(delta.items()))


def apply_delta(stable, entries, removed):
    """Stable's manifest with delta entries put in and removed keys taken out."""
    manifest = copy.deepcopy(stable)
    for key in removed:
        manifest.pop(key, None)
    for key, entry in entries.items():
        manifest[key] = entry
    return manifest


def carry_dates(history, previous, keys):
    """Keep a staged table's dates from the previous sync if it is unchanged.

    Each sync starts from stable's history, which would re-stamp every staged
    table with the time of the latest sync. A table whose fingerprint matches
    what the previous sync recorded keeps that sync's dates instead.
    """
    before = (previous or {}).get("tables", {})
    for key in keys:
        now, then = history["tables"].get(key), before.get(key)
        if now and then and now.get("fingerprint") == then.get("fingerprint"):
            for field in catalog_history.DATE_FIELDS:
                if field in then:
                    now[field] = then[field]
    return history


def hosted_here(manifest, repo):
    """Asset names a manifest downloads from the pre-release."""
    prefix = f"https://github.com/{repo}/releases/download/{TAG}/"
    return {gh_api.asset_name(e.get("repoConfig")) for e in (manifest or {}).values()
            if (e.get("repoConfig") or "").startswith(prefix)}


def zips_to_purge(asset_names, manifest, previous, repo):
    """Zips neither this manifest nor the previous one references."""
    keep = hosted_here(manifest, repo) | hosted_here(previous, repo)
    return sorted(n for n in asset_names if n.endswith(".zip") and n not in keep)


def fresh_in_delta(delta, previous):
    """Staged tables new since the previous sync: new to the delta or a new version."""
    before = (previous or {}).get("tables", {})
    return [k for k, v in delta.items()
            if v["change"] != "removed" and before.get(k) != v]


def notes(manifest, delta, keys=None):
    """Release-notes body in the format notify-discord-release.py reads."""
    keys = list(delta) if keys is None else keys

    def line(key):
        name = (manifest.get(key) or {}).get("name") or key
        return f"- [{name}]({TESTERS_URL.format(key=key)}) (`{key}`)"

    blocks = []
    for heading, change in (("## Newly added tables", "added"), ("## Updated tables:", "updated")):
        rows = [line(k) for k in keys if delta[k]["change"] == change]
        if rows:
            blocks.append("\n".join([heading] + rows))
    removed = [f"- `{k}`" for k in keys if delta[k]["change"] == "removed"]
    if removed:
        blocks.append("\n".join(["## Removed tables"] + removed))
    return "\n\n".join(blocks)


def md5(data):
    return hashlib.md5(data).hexdigest()


# --- Reading main -----------------------------------------------------------

def read_main(tables_dir=TABLES):
    """(trees, enabled) for the Wizard folders on the checked-out main."""
    import yaml
    import vpsdb

    trees = config_bundle.folder_trees("HEAD", tables_dir)
    wizard = {k: t for k, t in trees.items()
              if k.startswith("vpx-") and os.path.isfile(os.path.join(tables_dir, k, "table.yml"))}
    enabled = set()
    for key in wizard:
        with open(os.path.join(tables_dir, key, "table.yml")) as fh:
            data = yaml.safe_load(fh) or {}
        if not vpsdb.is_disabled(data):
            enabled.add(key)
    return wizard, enabled


def resolve(keys, trees, tables_dir=TABLES):
    """Manifest entries for keys from their table.yml, minus the release fields.

    Returns (entries, unresolved). A table VPSDB cannot resolve is left out,
    exactly as a full build would leave it out, and keeps stable's entry.
    """
    import contextlib
    import vpsdb

    files = [os.path.join(tables_dir, k, "table.yml") for k in keys]
    with contextlib.redirect_stdout(sys.stderr):
        meta = vpsdb.get_table_meta(files) if files else {}
    entries = {}
    for key in keys:
        entry = meta.get(key)
        if not entry:
            continue
        entry["name"] = vpsdb.process_title(entry["name"], entry["manufacturer"], entry["year"])
        entry["configVersion"] = trees[key][:7]
        entry["configFingerprint"] = config_bundle.fingerprint(f"{tables_dir}/{key}")[:16]
        entries[key] = entry
    return entries, sorted(set(keys) - set(entries))


# --- Sync -------------------------------------------------------------------

def load_release(api, release):
    """(manifest, delta, history, assets) of a release; empty pieces are None."""
    if not release:
        return None, None, None, []
    assets = api.assets(release["id"])
    parsed = []
    for name in ("manifest.json", "delta.json", "table-history.json"):
        raw = api.named_bytes(assets, name)
        parsed.append(json.loads(raw) if raw else None)
    return (*parsed, assets)


def ensure_release(api, sha):
    release = api.release_by_tag(TAG)
    if release:
        return release
    print(f"Creating the {TAG} release at {sha[:7]}")
    return api.call("POST", "releases", {
        "tag_name": TAG, "target_commitish": sha, "name": "Pre-release",
        "body": "Syncing...", "prerelease": True, "draft": False, "make_latest": "false",
    })


def build_mirror(repo, token, manifest, stable, delta, history_bytes, extras, tables_dir):
    """The manifest-testing commit: stable's mirror plus the delta's art."""
    fetched = mirror_tree.fetch_branches(repo, token, "manifest")
    tree = mirror_tree.TreeBuilder("origin/manifest" if fetched else None)
    staged = [k for k, v in delta.items() if v["change"] != "removed" and k in manifest]
    removed = [k for k, v in delta.items() if v["change"] == "removed"]

    vpinmdb_raw = mirror_tree.show("origin/manifest", "vpinmdb.json") if fetched else None
    vpinmdb = json.loads(vpinmdb_raw) if vpinmdb_raw else {}
    with tempfile.TemporaryDirectory() as tmp:
        if staged:
            boxart = os.path.join(tmp, "boxart")
            subprocess.run([sys.executable, os.path.join(SCRIPTS, "generate-boxart.py"),
                            "--tables-dir", tables_dir, "--out", boxart, "--only", *staged],
                           check=True)
            for name in os.listdir(boxart) if os.path.isdir(boxart) else []:
                with open(os.path.join(boxart, name), "rb") as fh:
                    tree.put(f"boxart/{name}", fh.read())

            delta_manifest = os.path.join(tmp, "delta-manifest.json")
            with open(delta_manifest, "w") as fh:
                json.dump({k: manifest[k] for k in staged}, fh)
            media = os.path.join(tmp, "vpinmdb")
            # Backglass and playfield art come from vpinmediadb. Losing it for a
            # run is cosmetic, so a failure keeps stable's copy, not the sync.
            fetched_media = subprocess.run(
                [sys.executable, os.path.join(SCRIPTS, "generate-vpinmdb-mirror.py"),
                 "--manifest", delta_manifest, "--out", media])
            index = os.path.join(media, "vpinmdb.json")
            if fetched_media.returncode == 0 and os.path.isfile(index):
                with open(index) as fh:
                    fresh = json.load(fh)
                for vid, record in fresh.items():
                    vpinmdb[vid] = record
                    tree.drop(f"media/{vid}")
                    tree.put_dir(os.path.join(media, "media", vid), f"media/{vid}")
            else:
                print("::warning::vpinmediadb art could not be mirrored; staged tables keep stable's")

    in_use = {e.get("vpsdbId") for e in manifest.values()}
    for key in removed:
        tree.drop(f"boxart/{key}.webp")
        vid = (stable.get(key) or {}).get("vpsdbId")
        if vid and vid not in in_use:
            tree.drop(f"media/{vid}")
            vpinmdb.pop(vid, None)

    tree.put("vpinmdb.json", json.dumps(vpinmdb, indent=2, sort_keys=True).encode())
    tree.put("manifest.json", json.dumps(manifest, indent=2).encode())
    tree.put("table-history.json", history_bytes)
    for name, data in extras.items():
        if data is not None:
            tree.put(name, data)
    return tree.commit(f"Catalog data as of {TAG} ({len(delta)} staged over stable)")


def sync(api, dry_run=False, out_dir="prerelease-out", tables_dir=TABLES):
    """Bring the pre-release in line with main. Returns a result dict."""
    sha = subprocess.run(["git", "rev-parse", "HEAD"], capture_output=True, text=True,
                         check=True).stdout.strip()
    stable = api.get("releases/latest")
    stable_assets = stable["assets"]
    stable_m = json.loads(api.named_bytes(stable_assets, "manifest.json"))
    stable_hist_raw = api.named_bytes(stable_assets, "table-history.json")
    if not stable_hist_raw:
        raise RuntimeError(f"stable {stable['tag_name']} has no table-history.json")
    stable_hist = json.loads(stable_hist_raw)

    release = api.release_by_tag(TAG)
    prev_m, prev_delta, prev_hist, assets = load_release(api, release)
    names = {a["name"] for a in assets}

    trees, enabled = read_main(tables_dir)
    delta = compute_delta(stable_m, trees, enabled)
    staged = [k for k, v in delta.items() if v["change"] != "removed"]
    entries, unresolved = resolve(staged, trees, tables_dir)
    for key in unresolved:
        delta.pop(key)
    removed = [k for k, v in delta.items() if v["change"] == "removed"]

    # Zips: reuse what an earlier sync uploaded, build the rest.
    to_build = []
    for key, entry in entries.items():
        name = zip_name(key, entry["configVersion"])
        entry["repoConfig"] = gh_api.asset_url(api.repo, TAG, name)
        before = (prev_m or {}).get(key) or {}
        if name in names and before.get("repoConfig") == entry["repoConfig"] \
                and before.get("repoConfigChecksum"):
            entry["repoConfigChecksum"] = before["repoConfigChecksum"]
        else:
            to_build.append(key)

    manifest = apply_delta(stable_m, entries, removed)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    extras = {}
    for name, path in EXTRAS.items():
        try:
            with open(path, "rb") as fh:
                extras[name] = fh.read()
        except FileNotFoundError:
            extras[name] = None
    record = {
        "schemaVersion": 1,
        "stable": stable["tag_name"],
        "main_sha": sha,
        "tables": delta,
        "unresolved": unresolved,
        "extras": {n: md5(d) for n, d in extras.items() if d is not None},
    }
    result = {"stable": stable["tag_name"], "main_sha": sha, "delta": delta,
              "unresolved": unresolved, "build": to_build, "changed": True,
              "fresh": fresh_in_delta(delta, prev_delta)}

    # Nothing to do when what testers would read is what they already read.
    if (not to_build and prev_delta and prev_m is not None
            and prev_delta.get("stable") == record["stable"]
            and prev_delta.get("tables") == delta
            and prev_delta.get("extras") == record["extras"]):
        print("The pre-release already matches main; nothing to sync.")
        result["changed"] = False
        return result

    os.makedirs(out_dir, exist_ok=True)
    if dry_run:
        for key in to_build:  # checksums are only known once zipped
            manifest[key]["repoConfigChecksum"] = "(built on sync)"
        with open(os.path.join(out_dir, "manifest.json"), "w") as fh:
            json.dump(manifest, fh, indent=2)
        with open(os.path.join(out_dir, "delta.json"), "w") as fh:
            json.dump(record, fh, indent=2)
        print(f"Dry run: wrote {out_dir}/manifest.json and delta.json; nothing uploaded.")
        return result

    release = ensure_release(api, sha)
    for key in to_build:
        name = zip_name(key, manifest[key]["configVersion"])
        with tempfile.TemporaryDirectory() as tmp:
            path, digest = config_bundle.build_zip(os.path.join(tables_dir, key),
                                                   os.path.join(tmp, key))
            stale = [a for a in api.assets(release["id"]) if a["name"] == name]
            for a in stale:  # an upload whose manifest never landed
                api.delete_asset(a["id"])
            with open(path, "rb") as fh:
                api.upload(release["id"], name, fh.read(), "application/zip")
        manifest[key]["repoConfigChecksum"] = digest
        print(f"  uploaded {name}")

    history = catalog_history.advance(stable_hist, manifest, TAG, now)
    history = carry_dates(history, prev_hist, list(entries))
    catalog_history.stamp(manifest, history)

    manifest_bytes = json.dumps(manifest, indent=2).encode()
    history_bytes = json.dumps(history, indent=2, sort_keys=True).encode()
    with open(os.path.join(out_dir, "manifest.json"), "wb") as fh:
        fh.write(manifest_bytes)

    # The testing mirror first, as with every catalog update: Table Manager
    # reads both, and the manifest is the switch.
    commit = build_mirror(api.repo, api.token, manifest, stable_m, delta, history_bytes,
                          extras, tables_dir)
    mirror_tree.push(api.repo, api.token, commit, "manifest-testing")
    print(f"manifest-testing -> {commit}")

    import release_meta
    meta = release_meta.build(api.repo, TAG, manifest, os.path.join(out_dir, "manifest.json"))
    files = [("table-history.json", history_bytes),
             ("release-meta.json", json.dumps(meta, indent=2, sort_keys=True).encode())]
    if extras["achievements.json"] is not None:
        files.append(("achievements.json", extras["achievements.json"]))
    files += [("delta.json", json.dumps(record, indent=2).encode()),
              ("manifest.json", manifest_bytes)]  # last: it is the switch
    for name, data in files:
        api.replace_asset(release["id"], api.assets(release["id"]), name, data)
        print(f"  published {name}")

    current = api.assets(release["id"])
    purge = zips_to_purge({a["name"] for a in current}, manifest, prev_m, api.repo)
    for a in current:
        if a["name"] in purge or a["name"].endswith(".next"):
            api.delete_asset(a["id"])
            print(f"  purged {a['name']}")
    result["purged"] = purge

    body = notes(manifest, delta) or "Nothing is staged: the pre-release matches stable."
    body += (f"\n\nStable `{stable['tag_name']}` plus {len(delta)} staged change(s), "
             f"synced from main at {sha[:7]}.\n")
    api.call("PATCH", f"releases/{release['id']}", {"body": body, "tag_name": TAG})

    if result["fresh"]:
        announce = dict(api.get(f"releases/{release['id']}"),
                        body=notes(manifest, delta, result["fresh"]))
        path = os.path.join(out_dir, "announce.json")
        with open(path, "w") as fh:
            json.dump(announce, fh)
        result["announce"] = path
    return result


def summary_markdown(result):
    delta = result["delta"]
    lines = [f"### Pre-release sync: {'updated' if result['changed'] else 'already current'}", "",
             f"Stable `{result['stable']}` + {len(delta)} staged change(s), main "
             f"`{result['main_sha'][:7]}`."]
    if delta:
        lines += ["", "| Table | Change | Version | Built this run |", "|---|---|---|---|"]
        for k, v in delta.items():
            lines.append(f"| `{k}` | {v['change']} | {v['configVersion'] or '-'} | "
                         f"{'yes' if k in result['build'] else ''} |")
    if result["unresolved"]:
        lines += ["", "**Not staged: VPSDB could not resolve** "
                  + ", ".join(f"`{k}`" for k in result["unresolved"])
                  + ". They keep stable's entry until fixed."]
    if result.get("purged"):
        lines += ["", "Purged: " + ", ".join(f"`{n}`" for n in result["purged"])]
    return "\n".join(lines) + "\n"


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("sync", help="bring the pre-release in line with main")
    s.add_argument("--dry-run", action="store_true", help="compute and write files; upload nothing")
    s.add_argument("--out-dir", default="prerelease-out")
    s.add_argument("--summary", help="append a markdown summary here")
    args = parser.parse_args(argv)

    result = sync(gh_api.Api.from_env(), args.dry_run, args.out_dir)
    text = summary_markdown(result)
    print(text)
    if args.summary:
        with open(args.summary, "a") as fh:
            fh.write(text)
    out = os.environ.get("GITHUB_OUTPUT")
    if out:
        with open(out, "a") as fh:
            fh.write(f"changed={str(result['changed']).lower()}\n")
            fh.write(f"announce={result.get('announce', '')}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
