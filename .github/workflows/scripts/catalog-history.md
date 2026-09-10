# Table discovery history

The release workflow publishes `table-history.json` alongside `manifest.json`.
The manifest includes `firstAvailableAt`, `firstAvailableRelease`, `updatedAt`,
and `updatedRelease` for each table. Generated history is never committed.

On the first run, the generator reconstructs history from the publishing
repository's own manifests and publication dates. Forks use their own releases;
upstream releases are never consulted. Releases without a `manifest.json` asset
are skipped because they did not deliver a Wizard catalog. Existing manifest
assets that cannot be downloaded or parsed still fail the build.
After the first run, each repository continues from its own released history
asset, replaying any intervening manifests. Reruns preserve dates; rebuilding
an older release excludes newer history.

Removed tables remain in history. Reintroductions and detected folder renames
preserve first arrival. Changes to a table's config contents or its component
versions and checksums advance the update date; release URLs and regenerated ZIP
bytes do not.

Two separate signals, deliberately:

* `configVersion` — the git tree id of the whole table folder. Drives device
  re-download, so a new launcher.png does reach cabinets.
* `configFingerprint` — a hash of the folder's *functional* files only
  (everything except `README.md`, `launcher.png`, `backglass.png`, `dmd.png`,
  `dmdframe.png`, `playfield.png`). This is what the history fingerprints, so
  "updated" means the table actually changed rather than its art being
  recompressed.

Both are content-derived rather than history-derived, which is what makes them
survive a repository flatten: rewriting history changes every commit sha, but a
table whose files are untouched keeps the same tree and blob ids.

`fingerprintVersion` records which definition a stored ledger was written under.
When CONTENT_FIELDS changes, bump `FINGERPRINT_VERSION`; `release_history`
re-baselines the stored fingerprints to the new definition and leaves the update
dates alone, instead of reporting every table as updated. A re-baseline is
silent about content, so confirm the release carrying it has no real table
changes.

For a read-only backfill preview:

```sh
python .github/workflows/scripts/catalog_history.py \
  --repo n-i-x/vpx-standalone-alp4k --output /tmp/table-history.json
python .github/workflows/scripts/generate-manifest.py vpx-mm \
  --history /tmp/table-history.json
```

Publish a release to trigger the workflow, or manually dispatch it with the
existing published release's tag to rebuild its assets. Draft releases are not
supported: the discovery timestamps represent publication dates.

Run history tests with:

```sh
python -m unittest discover -s .github/workflows/scripts -p test_catalog_history.py
```
