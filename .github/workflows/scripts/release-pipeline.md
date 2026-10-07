# Release pipeline: rolling pre-release and promotion

```
merge to main ──Sync Pre-release──▶ pre-release  = stable + delta (new / changed / removed tables)
                                         │
                       Promote Release ──┤ tables: all          → new stable vX.Y.Z, delta empties
                                         └ tables: vpx-a vpx-b  → new stable vX.Y.Z, the rest stays
```

* **Stable** releases (`vX.Y.Z`) are created only by Promote Release. They are
  never deleted: each stable manifest points back at older stable releases for
  tables that did not change.
* **The pre-release** is one release tagged `pre-release`, updated in place and
  never re-cut. Table Manager's testing tracks read it; stable devices never do.
* **The delta** is every Wizard table (a `tables/<key>/` folder with a
  `table.yml`) whose folder on main differs from stable:
  * **added**: on main, not in stable
  * **updated**: main's folder tree id is not stable's `configVersion`
  * **removed**: in stable, but deleted or `enabled: false` on main

  It is decided by folder trees alone. A table whose files are untouched keeps
  stable's entry, VPSDB metadata included, until its folder changes. A changed
  table that VPSDB cannot resolve stays out of the delta (it keeps stable's
  entry) and is listed as unresolved in the sync summary.

## Sync Pre-release

Runs on every push to main touching `tables/**`, `tm-config/**`,
`team_favorites.json` or `editors_picks.json`, after every promotion, and on
demand. Each run reconciles the pre-release to main:

1. Computes the delta against `/releases/latest`.
2. Zips only delta tables it has not zipped before. Pre-release zips are named
   by content, `vpx-x-<configVersion>.zip`, so an unchanged table is never
   rebuilt and a new version sits beside the old one.
3. Rebuilds the `manifest-testing` catalog mirror from the stable mirror,
   re-encoding art only for delta tables.
4. Publishes `table-history.json`, `release-meta.json`, `achievements.json`,
   `delta.json` and, last, `manifest.json`: the full testing manifest, stable
   with the delta applied. Each is swapped in under a temporary name and
   renamed, so it is missing for one API call at most.
5. Purges zips that neither this manifest nor the previous one references. The
   one-sync grace keeps a device that just read the old manifest working.
6. Posts to the testing Discord channel, only for tables new to the delta or
   with a new version in it.

When nothing changed it uploads nothing. A failed run leaves the previous
`manifest.json` in place; the next run reconciles everything, so there is no
cleanup step.

`delta.json`:

```jsonc
{
  "schemaVersion": 1,
  "stable": "v2.0.14",              // the stable release this delta is against
  "main_sha": "…",                  // the main commit it was synced from
  "tables": {"vpx-a": {"change": "updated", "configVersion": "a2a2a2a"},
             "vpx-gone": {"change": "removed", "configVersion": null}},
  "unresolved": [],                 // changed on main, but VPSDB could not resolve them
  "extras": {"achievements.json": "<md5>", "team_favorites.json": "<md5>", "editors_picks.json": "<md5>"}
}
```

Set the repository variable `PRERELEASE_SYNC` to `false` to turn syncing off
upstream, or to `true` to turn it on in a fork.

## Promote Release

Assembles a **new** stable release from the stable manifest plus the selected
delta entries. Nothing is rebuilt: each promoted zip is copied out of the
pre-release (md5 checked) and stored under its plain name, `vpx-x.zip`. The
stable `manifest` mirror takes the promoted tables' art from
`manifest-testing`, the same bytes testers saw. Afterwards the pre-release is
synced again, so what was promoted leaves the delta.

`tables` is `all` (every delta table) or a list of keys. Every table, in both
modes, must pass, or nothing is promoted:

* **not in the pre-release**: main and stable agree on it, or its change has
  not been synced yet. Tables never go straight to stable.
* **main has moved**: main's folder is not the version the pre-release built,
  or the table is gone, disabled or back on main. Wait for the sync.
* **already in stable**
* **unknown table**

The request also fails when there is no pre-release, when the pre-release was
synced against an older stable (a sync is pending), when `expected_prerelease`
no longer matches, or when an explicit `release_tag` is in use.

Non-table catalog data (`achievements.json`, `team_favorites.json`,
`editors_picks.json`) is promoted only with `all`; a partial promotion keeps
stable's.

The stable tag marks the main commit the check ran against. GitHub does not
let the workflow token tag a commit whose workflow files differ from the
default branch, so an older commit would not work anyway.

## Dry runs and the JSON contract

`dry_run` defaults to **true**. A dry run only checks, reads nothing but a
handful of API responses, and takes seconds. It writes the job summary and
uploads `promotion-check.json` as the `promotion-check` artifact. The run
**fails** when the request cannot be promoted.

```jsonc
{
  "promotable": true,
  "mode": "partial",                       // or "all"
  "errors": [],                            // one human-readable line per problem
  "prerelease": {"id": 1, "manifest_md5": "…", "main_sha": "…"},
  "stable": {"tag_name": "v2.0.14", "id": 2},
  "next_tag": "v2.0.15",
  "main_sha": "…",
  "tables": [{"key": "vpx-a", "change": "updated", "ok": true, "reason": "",
              "staged": "a2a2a2a", "main": "a2a2a2a", "stable": "aaaaaaa"}],
  "staged":    {"vpx-a": "updated", "vpx-b": "added"},   // everything in the pre-release
  "promote":   {"vpx-a": "updated"},
  "remaining": {"vpx-b": "added"}                        // what stays in the pre-release
}
```

A dry run with `tables: all` lists what is promotable.

## Bots

Give the bot a GitHub App installation token or a fine-grained token on this
repository with **Actions: read and write** (to dispatch and to read runs and
artifacts) and **Contents: read**. The workflows do the writing with their own
`GITHUB_TOKEN`.

1. Dispatch a dry run with a `request_id`, so the bot can find its run:

   ```sh
   gh api repos/LegendsUnchained/vpx-standalone-alp4k/actions/workflows/promote-release.yml/dispatches \
     -f ref=main -f 'inputs[tables]=vpx-a vpx-b' -f 'inputs[dry_run]=true' -f 'inputs[request_id]=abc123'
   ```

2. Find the run whose name ends in `[abc123]`
   (`GET /actions/workflows/promote-release.yml/runs?event=workflow_dispatch`),
   wait for it to complete, and download its `promotion-check` artifact.
3. Show the caller `tables`, `errors` and `next_tag`, and ask them to confirm.
4. Dispatch again with `dry_run=false` and
   `expected_prerelease=<prerelease.manifest_md5>` from the dry run. If the
   pre-release changed in between, the run refuses instead of promoting
   something the caller did not see.

## Concurrency

* **Syncs** (`prerelease-sync`) run one at a time. GitHub keeps one waiting run
  per group and replaces it with the newest, so a burst of merges ends in one
  extra sync of the newest main. Replaced runs show as cancelled.
* **Promotions** (`promotion`) have a group of their own, so a sync never
  cancels a waiting promotion. Two real promotions requested while one runs
  still collapse to the newest; a bot should treat a cancelled run as "not
  promoted" and ask again.
* A sync never deletes a zip the current manifest references, so it cannot pull
  a promoted table's zip out from under a running promotion. A sync that lands
  between a bot's dry run and its real run makes `expected_prerelease`
  mismatch: dry-run again and re-confirm.
* Dry runs never queue.

## Local use

```sh
GITHUB_REPOSITORY=LegendsUnchained/vpx-standalone-alp4k GH_TOKEN=$(gh auth token) \
  python .github/workflows/scripts/prerelease.py sync --dry-run     # needs requirements.txt
GITHUB_REPOSITORY=LegendsUnchained/vpx-standalone-alp4k GH_TOKEN=$(gh auth token) \
  python .github/workflows/scripts/promotion.py check --tables all
python -m unittest discover -s .github/workflows/scripts -p 'test_p*.py'
```
