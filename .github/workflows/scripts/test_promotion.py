import hashlib
import json
import unittest
from unittest.mock import patch

import promotion
from promotion import ALL, PRERELEASE, merge_manifest, next_tag, parse_tables, rehome, validate

REPO = 'test/catalog'


def url(tag, name):
    return f'https://github.com/{REPO}/releases/download/{tag}/{name}'


def entry(version, tag='v1.0.0', name=None, key='vpx-a'):
    return {'configVersion': version, 'repoConfig': url(tag, name or f'{key}.zip'),
            'repoConfigChecksum': f'md5-{version}'}


STABLE = {
    'vpx-a': entry('aaaaaaa', key='vpx-a'),
    'vpx-b': entry('bbbbbbb', key='vpx-b'),
    'vpx-gone': entry('ggggggg', key='vpx-gone'),
}
TESTING = {
    'vpx-a': entry('a2a2a2a', PRERELEASE, 'vpx-a-a2a2a2a.zip'),
    'vpx-b': entry('b2b2b2b', PRERELEASE, 'vpx-b-b2b2b2b.zip'),
    'vpx-new': entry('nnnnnnn', PRERELEASE, 'vpx-new-nnnnnnn.zip'),
}
DELTA = {
    'vpx-a': {'change': 'updated', 'configVersion': 'a2a2a2a'},
    'vpx-b': {'change': 'updated', 'configVersion': 'b2b2b2b'},
    'vpx-gone': {'change': 'removed', 'configVersion': None},
    'vpx-new': {'change': 'added', 'configVersion': 'nnnnnnn'},
}
MAIN = {'vpx-a': 'a2a2a2a' + '0' * 33, 'vpx-b': 'b3b3b3b' + '0' * 33,
        'vpx-new': 'nnnnnnn' + '0' * 33}


class ParseTests(unittest.TestCase):
    def test_separators_prefix_and_duplicates(self):
        self.assertEqual(parse_tables('vpx-a, b\nvpx-c  a tables/vpx-d'),
                         ['vpx-a', 'vpx-b', 'vpx-c', 'vpx-d'])

    def test_all(self):
        self.assertEqual(parse_tables(' ALL '), ALL)

    def test_all_with_keys_is_rejected(self):
        with self.assertRaises(ValueError):
            parse_tables('all vpx-a')

    def test_empty_is_rejected(self):
        with self.assertRaises(ValueError):
            parse_tables(' , ')


class ValidateTests(unittest.TestCase):
    def verdicts(self, keys, main=MAIN, disabled=frozenset(), stable=STABLE):
        return {r['key']: r for r in validate(keys, DELTA, TESTING, stable, main, disabled)}

    def test_matching_main_passes(self):
        v = self.verdicts(['vpx-a', 'vpx-new', 'vpx-gone'])
        self.assertTrue(all(r['ok'] for r in v.values()), v)

    def test_main_moved(self):
        self.assertIn('main has moved', self.verdicts(['vpx-b'])['vpx-b']['reason'])

    def test_gone_or_disabled_on_main(self):
        main = {k: t for k, t in MAIN.items() if k != 'vpx-a'}
        self.assertIn('gone or disabled', self.verdicts(['vpx-a'], main)['vpx-a']['reason'])
        self.assertIn('gone or disabled',
                      self.verdicts(['vpx-a'], disabled={'vpx-a'})['vpx-a']['reason'])

    def test_removal_back_on_main(self):
        main = dict(MAIN, **{'vpx-gone': 'g' * 40})
        self.assertIn('back on main', self.verdicts(['vpx-gone'], main)['vpx-gone']['reason'])
        self.assertTrue(self.verdicts(['vpx-gone'], main, {'vpx-gone'})['vpx-gone']['ok'])

    def test_not_in_prerelease_and_unknown(self):
        main = dict(MAIN, **{'vpx-same': 's' * 40})
        self.assertIn('not in the pre-release', self.verdicts(['vpx-same'], main)['vpx-same']['reason'])
        self.assertIn('unknown table', self.verdicts(['vpx-nope'])['vpx-nope']['reason'])

    def test_already_stable(self):
        stable = dict(STABLE, **{'vpx-a': entry('a2a2a2a')})
        self.assertIn('already in stable', self.verdicts(['vpx-a'], stable=stable)['vpx-a']['reason'])


class MergeTests(unittest.TestCase):
    def test_takes_only_promoted(self):
        merged = merge_manifest(STABLE, TESTING, {'vpx-a': 'updated', 'vpx-gone': 'removed'})
        self.assertEqual(merged['vpx-a']['configVersion'], 'a2a2a2a')
        self.assertEqual(merged['vpx-b'], STABLE['vpx-b'])
        self.assertNotIn('vpx-gone', merged)
        self.assertNotIn('vpx-new', merged)
        self.assertIn('vpx-gone', STABLE)

    def test_rehome_renames_to_plain_stable_names(self):
        merged = merge_manifest(STABLE, TESTING, {'vpx-a': 'updated', 'vpx-new': 'added'})
        copies = rehome(merged, REPO, 'v1.0.1')
        self.assertEqual(copies, {
            'vpx-a': ('vpx-a-a2a2a2a.zip', 'vpx-a.zip', 'md5-a2a2a2a'),
            'vpx-new': ('vpx-new-nnnnnnn.zip', 'vpx-new.zip', 'md5-nnnnnnn')})
        self.assertEqual(merged['vpx-a']['repoConfig'], url('v1.0.1', 'vpx-a.zip'))
        self.assertEqual(merged['vpx-b']['repoConfig'], url('v1.0.0', 'vpx-b.zip'))
        self.assertIn(PRERELEASE, TESTING['vpx-a']['repoConfig'])


class TagTests(unittest.TestCase):
    def test_bumps_patch(self):
        self.assertEqual(next_tag('v2.0.14', set()), 'v2.0.15')

    def test_skips_tags_in_use(self):
        self.assertEqual(next_tag('v2.0.14a', {'v2.0.15', 'v2.0.16'}), 'v2.0.17')

    def test_unreadable(self):
        with self.assertRaises(ValueError):
            next_tag('banana', set())


MANIFEST_BYTES = json.dumps(TESTING).encode()
MD5 = hashlib.md5(MANIFEST_BYTES).hexdigest()


class FakeApi:
    repo = REPO

    def __init__(self):
        self.delta = {'stable': 'v1.0.0', 'main_sha': 'f00d', 'tables': DELTA}
        self.pre = {'id': 9, 'assets': [{'name': 'manifest.json', 'id': 'm'},
                                        {'name': 'delta.json', 'id': 'd'}]}
        self.stable = {'tag_name': 'v1.0.0', 'id': 1, 'assets': [{'name': 'manifest.json', 'id': 's'}]}

    def release_by_tag(self, tag):
        return self.pre

    def named_bytes(self, assets, name):
        ids = {a['name']: a['id'] for a in assets}
        return {'m': MANIFEST_BYTES, 'd': json.dumps(self.delta).encode(),
                's': json.dumps(STABLE).encode()}.get(ids.get(name))

    def get_or_none(self, path):
        return self.stable if path == 'releases/latest' else None

    def paginate(self, path):
        return [self.stable]

    def exists(self, path):
        return False


class CheckTests(unittest.TestCase):
    def setUp(self):
        self.api = FakeApi()
        patcher = patch.multiple(promotion,
                                 main_tables=lambda api, branch: ('f00d', MAIN),
                                 disabled_on_main=lambda api, sha, keys: set())
        patcher.start()
        self.addCleanup(patcher.stop)

    def test_partial_passes_and_reports_remaining(self):
        r = promotion.check(self.api, 'a new')
        self.assertTrue(r['promotable'], r['errors'])
        self.assertEqual(r['next_tag'], 'v1.0.1')
        self.assertEqual(r['target_commitish'], 'f00d')
        self.assertEqual(r['promote'], {'vpx-a': 'updated', 'vpx-new': 'added'})
        self.assertEqual(r['remaining'], {'vpx-b': 'updated', 'vpx-gone': 'removed'})
        self.assertEqual(r['prerelease']['manifest_md5'], MD5)

    def test_one_bad_table_fails_the_request(self):
        r = promotion.check(self.api, 'a b')
        self.assertFalse(r['promotable'])
        self.assertEqual(len(r['errors']), 1)

    def test_all_checks_main_too(self):
        r = promotion.check(self.api, 'all')
        self.assertFalse(r['promotable'])  # vpx-b moved on main
        self.assertIn('vpx-b', r['errors'][0])

    def test_all_when_main_matches(self):
        with patch.object(promotion, 'main_tables',
                          lambda api, branch: ('f00d', dict(MAIN, **{'vpx-b': 'b2b2b2b' + '0' * 33}))):
            r = promotion.check(self.api, 'all')
        self.assertTrue(r['promotable'], r['errors'])
        self.assertEqual(r['remaining'], {})

    def test_expected_prerelease_mismatch(self):
        r = promotion.check(self.api, 'a', expected_prerelease='0' * 32)
        self.assertFalse(r['promotable'])
        self.assertIn('changed since the dry run', r['errors'][0])
        self.assertTrue(promotion.check(self.api, 'a', expected_prerelease=MD5)['promotable'])

    def test_sync_pending(self):
        self.api.delta['stable'] = 'v0.9.9'
        r = promotion.check(self.api, 'a')
        self.assertIn('sync is pending', r['errors'][0])

    def test_no_prerelease(self):
        self.api.pre = None
        self.assertIn('no pre-release', promotion.check(self.api, 'all')['errors'][0])

    def test_nothing_staged(self):
        self.api.delta['tables'] = {}
        self.assertIn('nothing is staged', promotion.check(self.api, 'all')['errors'][0])

    def test_explicit_tag_in_use(self):
        self.assertFalse(promotion.check(self.api, 'a', release_tag='v1.0.0')['promotable'])


if __name__ == '__main__':
    unittest.main()
