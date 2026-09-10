import copy
import unittest
import subprocess
import tempfile
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch
from catalog_history import (advance, stamp, fingerprint, table_renames, release_history, utc,
                             catalog_assets, rebaseline, FINGERPRINT_VERSION)

D1 = '2025-01-01T00:00:00Z'
D2 = '2026-09-01T00:00:00Z'
D3 = '2026-09-05T00:00:00Z'

def published_release(tag, date, *assets):
    return SimpleNamespace(tag_name=tag, published_at=utc(date), draft=False,
                           prerelease=False, get_assets=lambda: [
                               SimpleNamespace(name=name, browser_download_url=f'{tag}/{name}')
                               for name in assets])

def repository(*releases):
    return SimpleNamespace(full_name='test/catalog', get_releases=lambda: releases)

class HistoryTests(unittest.TestCase):
    def test_reuses_embedded_asset_metadata_without_paginating_table_zips(self):
        release = SimpleNamespace(raw_data={'assets': [
            {'name': 'table.zip', 'browser_download_url': 'zip'},
            {'name': 'manifest.json', 'browser_download_url': 'manifest'},
            {'name': 'table-history.json', 'browser_download_url': 'history'},
        ]})
        self.assertEqual([a.name for a in catalog_assets(release)],
                         ['manifest.json', 'table-history.json'])

    @patch('catalog_history.table_renames', return_value={})
    @patch('catalog_history.download_json')
    def test_fork_uses_its_own_first_release_and_never_reads_upstream(self, download, renames):
        class ForkRepository(SimpleNamespace):
            fork = True
            @property
            def source(self):
                raise AssertionError('Must not read upstream history')
        old = published_release('fork-v1', D2, 'manifest.json')
        current = published_release('fork-v2', D3)
        repo = ForkRepository(full_name='fork/catalog', get_releases=lambda: [current, old])
        download.return_value = {'a': {'configFingerprint': 'a'}}
        h = release_history(repo, current, {'a': {'configFingerprint': 'b'}})
        self.assertEqual(h['tables']['a']['firstAvailableAt'], D2)
        self.assertEqual(h['tables']['a']['firstAvailableRelease'], 'fork-v1')
        self.assertEqual(h['tables']['a']['updatedAt'], D3)
        download.assert_called_once_with('fork-v1/manifest.json')

    @patch('catalog_history.table_renames', return_value={})
    @patch('catalog_history.download_json')
    def test_first_rollout_bootstraps_release_assets_without_local_seed(self, download, renames):
        old = published_release('v1', D1, 'manifest.json')
        current = published_release('v2', D2)
        download.return_value = {'old': {'configFingerprint': 'a'}}
        h = release_history(repository(current, old), current,
                            {'old': {'configFingerprint': 'b'}, 'new': {'configFingerprint': 'c'}})
        self.assertEqual(h['tables']['old']['firstAvailableAt'], D1)
        self.assertEqual(h['tables']['old']['updatedAt'], D2)
        self.assertEqual(h['tables']['new']['firstAvailableAt'], D2)
        download.assert_called_once_with('v1/manifest.json')

    @patch('catalog_history.table_renames', return_value={})
    @patch('catalog_history.download_json')
    def test_resumes_asset_and_replays_missing_intermediate_history(self, download, renames):
        first = published_release('v1', D1, 'table-history.json')
        middle = published_release('v2', D2, 'manifest.json')
        current = published_release('v3', D3)
        seed = advance(None, {'old': {'configFingerprint': 'a'}}, 'v1', D1)
        manifest = {'old': {'configFingerprint': 'b'}, 'middle': {'configFingerprint': 'c'}}
        download.side_effect = [seed, manifest]
        h = release_history(repository(current, middle, first), current, manifest)
        self.assertEqual(h['tables']['old']['firstAvailableAt'], D1)
        self.assertEqual(h['tables']['old']['updatedAt'], D2)
        self.assertEqual(h['tables']['middle']['firstAvailableAt'], D2)
        self.assertEqual([call.args[0] for call in download.call_args_list],
                         ['v1/table-history.json', 'v2/manifest.json'])

    @patch('catalog_history.table_renames', return_value={})
    @patch('catalog_history.download_json')
    def test_rerun_uses_own_asset_and_ignores_future_releases(self, download, renames):
        current = published_release('v2', D2, 'table-history.json')
        future = published_release('v3', D3, 'table-history.json')
        tables = {'a': {'configFingerprint': 'a'}}
        seed = advance(advance(None, tables, 'v1', D1), tables, 'v2', D2)
        download.return_value = seed
        self.assertEqual(release_history(repository(future, current), current, tables), seed)
        download.assert_called_once_with('v2/table-history.json')

    @patch('catalog_history.table_renames', return_value={})
    @patch('catalog_history.download_json')
    def test_first_ever_release_needs_no_existing_assets(self, download, renames):
        current = published_release('v1', D1)
        tables = {'a': {'configFingerprint': 'a'}}
        self.assertEqual(release_history(repository(current), current, tables),
                         advance(None, tables, 'v1', D1))
        download.assert_not_called()

    @patch('catalog_history.table_renames', return_value={})
    @patch('catalog_history.download_json')
    def test_non_catalog_releases_are_skipped(self, download, renames):
        experiment = published_release('experiment', D1)
        old = published_release('v1', D2, 'manifest.json')
        current = published_release('v2', D3)
        tables = {'a': {'configFingerprint': 'a'}}
        download.return_value = tables
        h = release_history(repository(current, old, experiment), current, tables)
        self.assertEqual(h['tables']['a']['firstAvailableAt'], D2)
        download.assert_called_once_with('v1/manifest.json')

    @patch('catalog_history.download_json', side_effect=OSError('download failed'))
    def test_existing_manifest_download_failure_is_not_silently_skipped(self, download):
        old = published_release('v1', D1, 'manifest.json')
        current = published_release('v2', D2)
        with self.assertRaisesRegex(OSError, 'download failed'):
            release_history(repository(current, old), current, {'a': {}})

    @patch('catalog_history.download_json')
    def test_mismatched_history_asset_is_rejected(self, download):
        current = published_release('v2', D2, 'table-history.json')
        download.return_value = advance(None, {'a': {}}, 'v1', D1)
        with self.assertRaisesRegex(ValueError, 'Invalid history asset'):
            release_history(repository(current), current, {'a': {}})

    def test_update_keeps_arrival_and_rerun_is_idempotent(self):
        original = {'vpx-a': {'configFingerprint':'abc','tableChecksum':['a']}}
        h = advance(None, original, 'v1', D1)
        initial = copy.deepcopy(h)
        changed = {'vpx-a': {'configFingerprint':'def','tableChecksum':['b']}}
        h = advance(h, changed, 'v2', D2)
        self.assertEqual(h['tables']['vpx-a']['firstAvailableAt'], D1)
        self.assertEqual(h['tables']['vpx-a']['updatedAt'], D2)
        self.assertEqual(advance(h, changed, 'v2', D2), h)
        self.assertEqual(initial['tables']['vpx-a']['updatedAt'], D1)
        h = advance(h, changed, 'v3', D3)
        self.assertEqual(h['tables']['vpx-a']['updatedAt'], D2)

    def test_removed_disabled_renamed_and_reintroduced_tables(self):
        h = advance(None, {'vpx-old': {'configFingerprint':'a'},'vpx-disabled': {'enabled':False}}, 'v1', D1)
        h = advance(h, {}, 'v2', D2)
        self.assertNotIn('vpx-disabled', h['tables'])
        manifest = {'vpx-new': {'configFingerprint':'a'}}
        h = advance(h, manifest, 'v3', D3, {'vpx-new':'vpx-old'})
        stamp(manifest,h)
        self.assertEqual(manifest['vpx-new']['firstAvailableAt'],D1)
        self.assertNotIn('vpx-old',manifest)
        h = advance(h, {'vpx-old': {'configFingerprint':'a'}}, 'v3', D3)
        self.assertEqual(h['tables']['vpx-old']['firstAvailableAt'],D1)

    def test_release_urls_repacking_and_checksum_order_are_not_updates(self):
        a = {'configFingerprint':'a','tableChecksum':['a','b'],'repoConfig':'v1.zip','repoConfigChecksum':'one'}
        b = {'configFingerprint':'a','tableChecksum':['b','a'],'repoConfig':'v2.zip','repoConfigChecksum':'two'}
        self.assertEqual(fingerprint(a),fingerprint(b))
        h = advance(advance(None,{'a':a},'v1',D1),{'a':b},'v2',D2)
        self.assertEqual(h['tables']['a']['updatedAt'],D1)

    def test_detects_yaml_rename_in_real_git_history(self):
        with tempfile.TemporaryDirectory() as tmp:
            def git(*args):
                subprocess.run(['git', '-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', *args], cwd=tmp, check=True, capture_output=True)
            git('init')
            old = Path(tmp) / 'tables' / 'vpx-old'
            old.mkdir(parents=True)
            (old / 'table.yml').write_text('enabled: true\ntableVpsId: abc\n')
            git('add', '.')
            git('commit', '-m', 'first')
            git('tag', 'v1')
            git('mv', 'tables/vpx-old', 'tables/vpx-new')
            git('commit', '-m', 'rename')
            git('tag', 'v2')
            self.assertEqual(table_renames('v1', 'v2', tmp), {'vpx-new':'vpx-old'})

    def test_refuses_old_release_against_new_ledger(self):
        h = advance(None,{'a':{}},'v2',D2)
        with self.assertRaises(ValueError):
            advance(h,{'a':{}},'v1',D1)

    def test_presentation_only_change_does_not_advance_update_date(self):
        # configVersion covers the whole folder, so recompressed art moves it.
        # configFingerprint covers functional files only and must not move,
        # which is what keeps the Recently Updated feed honest.
        before = {'vpx-a': {'configVersion': 'aaaaaaa', 'configFingerprint': 'fn1'}}
        after = {'vpx-a': {'configVersion': 'bbbbbbb', 'configFingerprint': 'fn1'}}
        h = advance(None, before, 'v1', D1)
        h = advance(h, after, 'v2', D2)
        self.assertEqual(h['tables']['vpx-a']['updatedAt'], D1)
        self.assertEqual(h['tables']['vpx-a']['updatedRelease'], 'v1')

    def test_functional_change_still_advances_update_date(self):
        before = {'vpx-a': {'configVersion': 'aaaaaaa', 'configFingerprint': 'fn1'}}
        after = {'vpx-a': {'configVersion': 'aaaaaaa', 'configFingerprint': 'fn2'}}
        h = advance(None, before, 'v1', D1)
        h = advance(h, after, 'v2', D2)
        self.assertEqual(h['tables']['vpx-a']['updatedAt'], D2)

    def test_rebaseline_adopts_new_scheme_without_touching_dates(self):
        manifest = {'vpx-a': {'configFingerprint': 'fn1'}}
        h = advance(None, manifest, 'v1', D1)
        # Simulate a ledger written under an older fingerprint definition.
        stale = copy.deepcopy(h)
        stale['fingerprintVersion'] = FINGERPRINT_VERSION - 1
        stale['tables']['vpx-a']['fingerprint'] = 'computed-under-the-old-scheme'

        rebased = rebaseline(stale, manifest)
        self.assertEqual(rebased['fingerprintVersion'], FINGERPRINT_VERSION)
        self.assertEqual(rebased['tables']['vpx-a']['fingerprint'], fingerprint(manifest['vpx-a']))
        self.assertEqual(rebased['tables']['vpx-a']['updatedAt'], D1)
        # ...and the next release then reports no change at all.
        nxt = advance(rebased, manifest, 'v2', D2)
        self.assertEqual(nxt['tables']['vpx-a']['updatedAt'], D1)
        self.assertEqual(nxt['tables']['vpx-a']['updatedRelease'], 'v1')

    def test_rebaseline_is_a_noop_when_already_current(self):
        manifest = {'vpx-a': {'configFingerprint': 'fn1'}}
        h = advance(None, manifest, 'v1', D1)
        self.assertIs(rebaseline(h, manifest), h)

if __name__ == '__main__': unittest.main()
