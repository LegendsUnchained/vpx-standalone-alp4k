#!/usr/bin/env python3
"""Promote tables from the rolling pre-release into a new stable release.

The pre-release (tag `pre-release`, kept in step with main by prerelease.py)
holds the delta between main and stable. This assembles a NEW stable release
out of it, from every staged table (`all`) or from the ones named:

    stable manifest + the selected delta entries  ->  new stable vX.Y.Z

Nothing is zipped. Each promoted table's zip is copied byte for byte out of
the pre-release, md5 checked against the manifest, and stored in the stable
release under its plain name (vpx-x.zip). Entries that already point at an
older stable release keep their URLs: stable releases are never deleted, which
is what lets an incremental release reference them.

Every table promoted, in every mode, must still match main: its folder's tree
id on main has to be the configVersion the pre-release built. So a table never
reaches stable without having been in the pre-release, as it is on main now.

Subcommands, in pipeline order:

    check          Validate a request. Standard library only, a few API reads,
                   so a dry run takes seconds.
    assemble       Create the stable draft and fill it.
    finalize-body  Append the promotion summary to the generated notes.
    mirror         Build the stable `manifest` branch commit.

See release-pipeline.md for the rules and the dry-run JSON contract.
"""
import argparse
import base64
import copy
import hashlib
import json
import os
import re
import sys
import urllib.parse
from concurrent.futures import ThreadPoolExecutor

import gh_api
import mirror_tree

PRERELEASE = "pre-release"
ALL = "all"


# --- Pure logic (unit tested) -----------------------------------------------

def parse_tables(text):
    """`all`, or table keys separated by commas, whitespace or newlines.

    A key without the vpx- prefix gets it; duplicates are dropped, order kept.
    """
    tokens = [t for t in re.split(r"[\s,]+", (text or "").strip()) if t]
    if not tokens:
        raise ValueError("no tables given; pass `all` or one or more table keys")
    if any(t.lower() == ALL for t in tokens):
        if len(tokens) > 1:
            raise ValueError("`all` cannot be combined with table keys")
        return ALL
    keys = []
    for token in tokens:
        token = token.strip("/").split("/")[-1]  # tolerate tables/vpx-foo
        key = token if token.startswith("vpx-") else f"vpx-{token}"
        if key not in keys:
            keys.append(key)
    return keys


DISABLED_RE = re.compile(r"^enabled:\s*false\s*(#.*)?$", re.MULTILINE)


def validate(requested, delta, testing, stable, main_trees, main_disabled=frozenset()):
    """Per-table verdicts.

    delta is the pre-release's delta.json "tables"; main_trees maps every
    tables/<key> folder on main to its tree id; main_disabled holds the keys
    whose table.yml on main says enabled: false.
    """
    verdicts = []
    for key in requested:
        staged = delta.get(key) or {}
        change = staged.get("change")
        tree = main_trees.get(key)
        row = {
            "key": key,
            "change": change,
            "staged": staged.get("configVersion"),
            "stable": (stable.get(key) or {}).get("configVersion"),
            "main": (tree or "")[:7] or None,
            "ok": False,
            "reason": "",
        }
        if change is None:
            if key in stable or key in main_trees:
                row["reason"] = ("not in the pre-release: main and stable agree on this table, "
                                 "or its change has not been synced yet.")
            else:
                row["reason"] = "unknown table: no such folder on main, in the pre-release or in stable."
        elif change == "removed":
            if tree is not None and key not in main_disabled:
                row["reason"] = "main has moved: the table is back on main. Wait for the pre-release sync."
            else:
                row["ok"] = True
        elif tree is None or key in main_disabled:
            row["reason"] = "main has moved: the table is gone or disabled on main. Wait for the pre-release sync."
        elif not row["staged"] or not tree.startswith(row["staged"]):
            row["reason"] = (f"main has moved: the pre-release has {row['staged']}, main has "
                             f"{tree[:7]}. Wait for the pre-release sync.")
        elif key not in testing:
            row["reason"] = "the pre-release manifest has no entry for it. Re-run the pre-release sync."
        elif row["stable"] == row["staged"]:
            row["reason"] = "already in stable."
        else:
            row["ok"] = True
        verdicts.append(row)
    return verdicts


def merge_manifest(stable, testing, promote):
    """The new stable manifest. promote is {key: change} of what moves."""
    merged = copy.deepcopy(stable)
    for key, change in promote.items():
        if change == "removed":
            merged.pop(key, None)
        else:
            merged[key] = copy.deepcopy(testing[key])
    return merged


def rehome(manifest, repo, new_tag):
    """Point entries hosted on the pre-release at the new stable release.

    Returns {key: (source asset name, stable asset name, md5)} of the zips to
    copy. Stable keeps plain vpx-x.zip names; the pre-release's names carry the
    config version so they can sit side by side while a sync rolls over.
    """
    prefix = f"/releases/download/{PRERELEASE}/"
    copies = {}
    for key, entry in manifest.items():
        url = entry.get("repoConfig") or ""
        if prefix not in url:
            continue
        stable_name = f"{key}.zip"
        copies[key] = (gh_api.asset_name(url), stable_name, entry.get("repoConfigChecksum"))
        entry["repoConfig"] = gh_api.asset_url(repo, new_tag, stable_name)
    return copies


VERSION_RE = re.compile(r"^v?(\d+)\.(\d+)\.(\d+)")


def next_tag(latest, taken):
    """The next free patch after the latest stable tag."""
    m = VERSION_RE.match(latest or "")
    if not m:
        raise ValueError(f"cannot read a version from latest stable '{latest}'; pass release_tag")
    major, minor, patch = (int(x) for x in m.groups())
    while True:
        patch += 1
        tag = f"v{major}.{minor}.{patch}"
        if tag not in taken:
            return tag


# --- check ------------------------------------------------------------------

def main_tables(api, branch):
    """(commit sha, {key: tree id}) for tables/ on the branch: three API reads."""
    sha = api.get(f"commits/{urllib.parse.quote(branch)}")["sha"]
    root = api.get(f"git/trees/{sha}")
    tables = next((t for t in root["tree"] if t["path"] == "tables" and t["type"] == "tree"), None)
    if tables is None:
        raise RuntimeError(f"no tables/ folder on {branch}")
    listing = api.get(f"git/trees/{tables['sha']}")
    if listing.get("truncated"):
        raise RuntimeError("tables/ listing was truncated by the API")
    return sha, {t["path"]: t["sha"] for t in listing["tree"]
                 if t["type"] == "tree" and t["path"].startswith("vpx-")}


def disabled_on_main(api, sha, keys):
    """Which of keys say enabled: false in their table.yml on main."""
    out = set()
    for key in keys:
        meta = api.get_or_none(f"contents/tables/{key}/table.yml?ref={sha}")
        if meta and DISABLED_RE.search(base64.b64decode(meta["content"]).decode("utf-8", "replace")):
            out.add(key)
    return out


def check(api, tables_text, branch="main", expected_prerelease="", release_tag=""):
    result = {"promotable": False, "errors": [], "tables": [], "staged": {},
              "promote": {}, "remaining": {}, "repo": api.repo}
    errors = result["errors"]
    try:
        requested = parse_tables(tables_text)
    except ValueError as e:
        errors.append(str(e))
        return result
    result["mode"] = ALL if requested == ALL else "partial"

    pre = api.release_by_tag(PRERELEASE)
    if pre is None:
        errors.append("there is no pre-release. Run 'Sync Pre-release' first.")
        return result
    raw_manifest = api.named_bytes(pre["assets"], "manifest.json")
    raw_delta = api.named_bytes(pre["assets"], "delta.json")
    if raw_manifest is None or raw_delta is None:
        errors.append("the pre-release has no manifest.json or delta.json. Re-run 'Sync Pre-release'.")
        return result
    checksum = hashlib.md5(raw_manifest).hexdigest()
    testing, delta_record = json.loads(raw_manifest), json.loads(raw_delta)
    delta = delta_record.get("tables") or {}
    result["prerelease"] = {"id": pre["id"], "manifest_md5": checksum,
                            "main_sha": delta_record.get("main_sha")}
    result["staged"] = {k: v["change"] for k, v in delta.items()}
    if expected_prerelease and expected_prerelease != checksum:
        errors.append("the pre-release changed since the dry run (manifest "
                      f"{checksum}, expected {expected_prerelease}). Check again.")
        return result

    stable = api.get_or_none("releases/latest")
    stable_m = {}
    if stable:
        result["stable"] = {"tag_name": stable["tag_name"], "id": stable["id"]}
        stable_m = json.loads(api.named_bytes(stable["assets"], "manifest.json") or b"{}")
    if delta_record.get("stable") != (stable or {}).get("tag_name"):
        errors.append(f"the pre-release was synced against {delta_record.get('stable')}, but "
                      f"stable is {(stable or {}).get('tag_name')}: a sync is pending. Try again shortly.")
        return result

    taken = {r["tag_name"] for r in api.paginate("releases")}
    if release_tag:
        if release_tag in taken or api.exists(f"git/ref/tags/{urllib.parse.quote(release_tag)}"):
            errors.append(f"tag {release_tag} is already in use")
        new_tag = release_tag
    else:
        try:
            new_tag = next_tag(stable["tag_name"] if stable else "v0.0.0", taken)
            while api.exists(f"git/ref/tags/{new_tag}"):
                taken.add(new_tag)
                new_tag = next_tag(new_tag, taken)
        except ValueError as e:
            errors.append(str(e))
            return result
    result["next_tag"] = new_tag

    keys = list(delta) if requested == ALL else requested
    if not keys:
        errors.append("nothing is staged: the pre-release matches stable.")
    # The stable tag marks the main commit checked here. The workflow token
    # may not create a tag on a commit whose workflow files differ from the
    # default branch, so an older commit is not an option anyway.
    sha, trees = main_tables(api, branch)
    disabled = disabled_on_main(api, sha, [k for k in keys if k in trees])
    result["main_sha"] = sha
    result["target_commitish"] = sha
    result["tables"] = validate(keys, delta, testing, stable_m, trees, disabled)
    for row in result["tables"]:
        if not row["ok"]:
            errors.append(f"{row['key']}: {row['reason']}")
    promote = {r["key"]: r["change"] for r in result["tables"] if r["ok"]}
    result["promote"] = promote
    result["remaining"] = {k: v["change"] for k, v in delta.items() if k not in promote}
    result["promotable"] = not errors
    return result


def summary_markdown(result):
    verdict = "can be promoted" if result.get("promotable") else "cannot be promoted"
    stable = (result.get("stable") or {}).get("tag_name", "none")
    lines = [f"### Promotion check: {verdict}", "",
             f"Pre-release → new stable `{result.get('next_tag', '?')}` "
             f"(current stable `{stable}`), mode `{result.get('mode', '?')}`."]
    if result.get("errors"):
        lines += ["", "**Problems**", ""] + [f"- {e}" for e in result["errors"]]
    if result.get("tables"):
        lines += ["", "| Table | Change | Pre-release | Main | Stable | OK | Reason |",
                  "|---|---|---|---|---|---|---|"]
        for r in result["tables"]:
            lines.append(f"| `{r['key']}` | {r['change'] or '-'} | {r['staged'] or '-'} | "
                         f"{r['main'] or '-'} | {r['stable'] or '-'} | "
                         f"{'yes' if r['ok'] else 'no'} | {r['reason']} |")
    lines += ["", f"Left in the pre-release afterwards: {len(result.get('remaining') or {})} table(s)."]
    if result.get("prerelease"):
        lines += ["", f"Pre-release manifest md5 `{result['prerelease']['manifest_md5']}` "
                  "(pass as expected_prerelease to promote exactly this)."]
    return "\n".join(lines) + "\n"


# --- assemble ---------------------------------------------------------------

def assemble(api, plan, out_dir):
    """Create the stable draft and fill it. Returns the draft release."""
    from github import Github, Auth  # only the real run needs PyGithub
    import catalog_history
    import release_meta

    new_tag = plan["next_tag"]
    if any(r["tag_name"] == new_tag for r in api.paginate("releases")):
        raise RuntimeError(f"a release for {new_tag} already exists; delete it or pick another tag")
    pre = api.release_by_tag(PRERELEASE)
    pre_assets = api.assets(pre["id"])
    raw_manifest = api.named_bytes(pre_assets, "manifest.json")
    if hashlib.md5(raw_manifest).hexdigest() != plan["prerelease"]["manifest_md5"]:
        raise RuntimeError("the pre-release changed since the check ran; run again")
    testing = json.loads(raw_manifest)
    stable = api.get(f"releases/{plan['stable']['id']}") if plan.get("stable") else None
    stable_m = json.loads(api.named_bytes(stable["assets"], "manifest.json")) if stable else {}

    merged = merge_manifest(stable_m, testing, plan["promote"])
    copies = rehome(merged, api.repo, new_tag)

    draft = api.call("POST", "releases", {
        "tag_name": new_tag,
        "target_commitish": plan["target_commitish"],
        "name": f"Update {new_tag.lstrip('v')}",
        "body": "Building...",
        "draft": True,
        "prerelease": False,
    })
    print(f"Created draft {new_tag} (id {draft['id']}) at {plan['target_commitish']}")

    by_name = {a["name"]: a for a in pre_assets}

    def copy_one(item):
        source, dest, want = item[1]
        asset = by_name.get(source)
        if asset is None:
            raise RuntimeError(f"{source} is missing from the pre-release")
        data = api.asset_bytes(asset["id"])
        got = hashlib.md5(data).hexdigest()
        if want and got != want:
            raise RuntimeError(f"{source}: md5 {got} does not match the manifest's {want}")
        api.upload(draft["id"], dest, data, "application/zip")
        print(f"  copied {source} -> {dest} ({len(data)} bytes, md5 ok)")

    print(f"Copying {len(copies)} config bundle(s) from the pre-release")
    with ThreadPoolExecutor(max_workers=4) as pool:
        list(pool.map(copy_one, sorted(copies.items())))

    # History from stable releases only, so the stamps name the real tag.
    gh = Github(auth=Auth.Token(api.token))
    repo = gh.get_repo(api.repo)
    history = catalog_history.release_history(repo, repo.get_release(draft["id"]), merged)
    catalog_history.stamp(merged, history)

    os.makedirs(out_dir, exist_ok=True)
    paths = {name: os.path.join(out_dir, name) for name in
             ("manifest.json", "table-history.json", "release-meta.json", "achievements.json")}
    with open(paths["table-history.json"], "w") as f:
        json.dump(history, f, indent=2, sort_keys=True)
    with open(paths["manifest.json"], "w") as f:
        json.dump(merged, f, indent=2)
    with open(paths["release-meta.json"], "w") as f:
        json.dump(release_meta.build(api.repo, new_tag, merged, paths["manifest.json"]),
                  f, indent=2, sort_keys=True)

    # Non-table catalog data rides only `all`; a partial promotion keeps stable's.
    source_assets = pre_assets if plan["mode"] == ALL or stable is None else stable["assets"]
    achievements = api.named_bytes(source_assets, "achievements.json")
    if achievements is not None:
        with open(paths["achievements.json"], "wb") as f:
            f.write(achievements)
    else:
        print("::warning::no achievements.json to publish")
        del paths["achievements.json"]

    for name, path in paths.items():
        with open(path, "rb") as f:
            api.upload(draft["id"], name, f.read(), "application/json")
        print(f"  uploaded {name}")
    return draft


def finalize_body(api, release_id, plan):
    release = api.get(f"releases/{release_id}")
    body = release.get("body") or ""
    if body.strip() == "Building...":
        body = ""
    removed = [k for k, c in plan["promote"].items() if c == "removed"]
    scope = ("everything in the pre-release" if plan["mode"] == ALL
             else f"{len(plan['promote'])} table(s)")
    blocks = [body.strip()] if body.strip() else []
    if removed:
        blocks.append("\n".join(["## Removed tables"] + [f"- `{k}`" for k in removed]))
    blocks.append(f"Promoted from the pre-release: {scope}, checked against main at "
                  f"{plan['target_commitish'][:7]}.")
    body = "\n\n".join(blocks) + "\n"
    # tag_name has to be repeated: a PATCH to a draft that leaves it out drops
    # the draft's tag, and it then publishes as untagged-<hash>.
    api.call("PATCH", f"releases/{release_id}", {
        "body": body, "tag_name": release["tag_name"],
        "target_commitish": release["target_commitish"]})
    print(body)


# --- mirror -----------------------------------------------------------------

def mirror(plan, data_dir, stable_manifest, testing_ref="origin/manifest-testing",
           stable_ref="origin/manifest"):
    """The new stable catalog commit (not pushed).

    Stable's mirror with the promoted tables' box art and media taken from the
    testing mirror, the very blobs testers were served. Nothing is re-encoded.
    """
    want = plan["prerelease"]["manifest_md5"]
    got = hashlib.md5(mirror_tree.show(testing_ref, "manifest.json") or b"").hexdigest()
    if got != want:
        raise RuntimeError(f"{testing_ref} is not the pre-release's catalog (manifest {got}, "
                           f"pre-release {want}); re-run 'Sync Pre-release' first")

    tree = mirror_tree.TreeBuilder(stable_ref)
    with open(os.path.join(data_dir, "manifest.json")) as fh:
        merged = json.load(fh)
    vpinmdb = json.loads(mirror_tree.show(stable_ref, "vpinmdb.json") or b"{}")
    testing_vpinmdb = json.loads(mirror_tree.show(testing_ref, "vpinmdb.json") or b"{}")
    in_use = {e.get("vpsdbId") for e in merged.values()}
    for key, change in plan["promote"].items():
        if change == "removed":
            tree.drop(f"boxart/{key}.webp")
            vid = (stable_manifest.get(key) or {}).get("vpsdbId")
            if vid and vid not in in_use:
                tree.drop(f"media/{vid}")
                vpinmdb.pop(vid, None)
            continue
        tree.take(testing_ref, f"boxart/{key}.webp")
        vid = merged[key].get("vpsdbId")
        if vid and vid in testing_vpinmdb:
            tree.take(testing_ref, f"media/{vid}")
            vpinmdb[vid] = testing_vpinmdb[vid]
    tree.put("vpinmdb.json", json.dumps(vpinmdb, indent=2, sort_keys=True).encode())

    for name in ("manifest.json", "table-history.json", "achievements.json"):
        path = os.path.join(data_dir, name)
        if os.path.exists(path):
            with open(path, "rb") as fh:
                tree.put(name, fh.read())
    if plan["mode"] == ALL:
        for name in ("team_favorites.json", "editors_picks.json"):
            tree.take(testing_ref, name)
    commit = tree.commit(f"Catalog data as of release {plan['next_tag']}")
    print(f"catalog commit {commit}")
    return commit


# --- CLI --------------------------------------------------------------------

def _output(**values):
    path = os.environ.get("GITHUB_OUTPUT")
    if not path:
        return
    with open(path, "a") as f:
        for k, v in values.items():
            f.write(f"{k}={v}\n")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="cmd", required=True)

    c = sub.add_parser("check", help="validate a promotion request")
    c.add_argument("--tables", required=True)
    c.add_argument("--branch", default="main")
    c.add_argument("--expected-prerelease", default="")
    c.add_argument("--release-tag", default="")
    c.add_argument("--out", default="promotion-check.json")
    c.add_argument("--summary", help="append a markdown summary here")

    a = sub.add_parser("assemble", help="create and fill the stable draft")
    a.add_argument("--plan", required=True)
    a.add_argument("--out-dir", default="promotion-out")

    f = sub.add_parser("finalize-body", help="append the promotion summary")
    f.add_argument("--plan", required=True)
    f.add_argument("--release-id", required=True)

    m = sub.add_parser("mirror", help="build the stable catalog commit")
    m.add_argument("--plan", required=True)
    m.add_argument("--data-dir", default="promotion-out")

    args = parser.parse_args(argv)
    api = gh_api.Api.from_env()

    if args.cmd == "check":
        result = check(api, args.tables, args.branch, args.expected_prerelease, args.release_tag)
        with open(args.out, "w") as fh:
            json.dump(result, fh, indent=2)
        text = summary_markdown(result)
        print(text)
        if args.summary:
            with open(args.summary, "a") as fh:
                fh.write(text)
        _output(promotable=str(result["promotable"]).lower(), tag=result.get("next_tag", ""))
        return 0 if result["promotable"] else 1

    with open(args.plan) as fh:
        plan = json.load(fh)
    if not plan.get("promotable"):
        sys.exit("the plan is not promotable")
    if args.cmd == "assemble":
        draft = assemble(api, plan, args.out_dir)
        _output(id=draft["id"], tag=draft["tag_name"])
    elif args.cmd == "finalize-body":
        finalize_body(api, args.release_id, plan)
    elif args.cmd == "mirror":
        stable_m = {}
        if plan.get("stable"):
            stable = api.get(f"releases/{plan['stable']['id']}")
            stable_m = json.loads(api.named_bytes(stable["assets"], "manifest.json"))
        _output(commit=mirror(plan, args.data_dir, stable_m))
    return 0


if __name__ == "__main__":
    sys.exit(main())
