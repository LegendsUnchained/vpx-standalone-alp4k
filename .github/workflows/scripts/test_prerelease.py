import subprocess
import tempfile
import os
import unittest
from pathlib import Path

import config_bundle
import mirror_tree
from prerelease import (TAG, apply_delta, carry_dates, compute_delta, fresh_in_delta, notes,
                        zip_name, zips_to_purge)

REPO = 'test/catalog'


def url(tag, name):
    return f'https://github.com/{REPO}/releases/download/{tag}/{name}'


STABLE = {
    'vpx-a': {'configVersion': 'aaaaaaa', 'name': 'A', 'repoConfig': url('v1', 'vpx-a.zip')},
    'vpx-b': {'configVersion': 'bbbbbbb', 'name': 'B', 'repoConfig': url('v1', 'vpx-b.zip')},
    'vpx-gone': {'configVersion': 'ggggggg', 'name': 'Gone', 'repoConfig': url('v1', 'vpx-gone.zip')},
    'vpx-off': {'configVersion': 'ooooooo', 'name': 'Off', 'repoConfig': url('v1', 'vpx-off.zip')},
}
TREES = {'vpx-a': 'aaaaaaa' + '1' * 33, 'vpx-b': 'b2b2b2b' + '1' * 33,
         'vpx-new': 'nnnnnnn' + '1' * 33, 'vpx-off': 'o2o2o2o' + '1' * 33,
         'vpx-never': 'eeeeeee' + '1' * 33}
ENABLED = {'vpx-a', 'vpx-b', 'vpx-new'}  # vpx-off and vpx-never say enabled: false


class DeltaTests(unittest.TestCase):
    def test_added_updated_removed_and_disabled(self):
        self.assertEqual(compute_delta(STABLE, TREES, ENABLED), {
            'vpx-b': {'change': 'updated', 'configVersion': 'b2b2b2b'},
            'vpx-gone': {'change': 'removed', 'configVersion': None},
            'vpx-new': {'change': 'added', 'configVersion': 'nnnnnnn'},
            'vpx-off': {'change': 'removed', 'configVersion': None},
        })

    def test_unchanged_main_has_no_delta(self):
        stable = {'vpx-a': STABLE['vpx-a']}
        self.assertEqual(compute_delta(stable, {'vpx-a': TREES['vpx-a']}, {'vpx-a'}), {})

    def test_apply(self):
        entry = {'configVersion': 'b2b2b2b', 'repoConfig': url(TAG, zip_name('vpx-b', 'b2b2b2b'))}
        out = apply_delta(STABLE, {'vpx-b': entry}, ['vpx-gone'])
        self.assertEqual(out['vpx-b'], entry)
        self.assertNotIn('vpx-gone', out)
        self.assertEqual(out['vpx-a'], STABLE['vpx-a'])
        self.assertIn('vpx-gone', STABLE)  # not mutated


class HistoryTests(unittest.TestCase):
    def test_unchanged_staged_table_keeps_previous_sync_dates(self):
        now = {'tables': {'vpx-b': {'fingerprint': 'f', 'updatedAt': 'NOW', 'updatedRelease': TAG},
                          'vpx-new': {'fingerprint': 'n2', 'updatedAt': 'NOW'}}}
        before = {'tables': {'vpx-b': {'fingerprint': 'f', 'updatedAt': 'THEN', 'updatedRelease': TAG},
                             'vpx-new': {'fingerprint': 'n1', 'updatedAt': 'THEN'}}}
        out = carry_dates(now, before, ['vpx-b', 'vpx-new'])
        self.assertEqual(out['tables']['vpx-b']['updatedAt'], 'THEN')
        self.assertEqual(out['tables']['vpx-new']['updatedAt'], 'NOW')

    def test_no_previous_sync(self):
        now = {'tables': {'vpx-b': {'fingerprint': 'f', 'updatedAt': 'NOW'}}}
        self.assertEqual(carry_dates(now, None, ['vpx-b'])['tables']['vpx-b']['updatedAt'], 'NOW')


class PurgeTests(unittest.TestCase):
    def test_keeps_this_and_the_previous_generation(self):
        current = {'vpx-b': {'repoConfig': url(TAG, 'vpx-b-b3b3b3b.zip')},
                   'vpx-a': {'repoConfig': url('v1', 'vpx-a.zip')}}
        previous = {'vpx-b': {'repoConfig': url(TAG, 'vpx-b-b2b2b2b.zip')}}
        assets = ['vpx-b-b1b1b1b.zip', 'vpx-b-b2b2b2b.zip', 'vpx-b-b3b3b3b.zip',
                  'vpx-x-xxxxxxx.zip', 'manifest.json', 'delta.json']
        self.assertEqual(zips_to_purge(assets, current, previous, REPO),
                         ['vpx-b-b1b1b1b.zip', 'vpx-x-xxxxxxx.zip'])

    def test_first_sync(self):
        self.assertEqual(zips_to_purge(['vpx-a-1.zip'], {}, None, REPO), ['vpx-a-1.zip'])


class AnnounceTests(unittest.TestCase):
    def test_only_new_or_reversioned_tables(self):
        before = {'tables': {'vpx-b': {'change': 'updated', 'configVersion': 'b2b2b2b'},
                             'vpx-c': {'change': 'updated', 'configVersion': 'c1c1c1c'}}}
        delta = {'vpx-b': {'change': 'updated', 'configVersion': 'b2b2b2b'},
                 'vpx-c': {'change': 'updated', 'configVersion': 'c2c2c2c'},
                 'vpx-new': {'change': 'added', 'configVersion': 'nnnnnnn'},
                 'vpx-gone': {'change': 'removed', 'configVersion': None}}
        self.assertEqual(fresh_in_delta(delta, before), ['vpx-c', 'vpx-new'])

    def test_notes_format_matches_the_discord_reader(self):
        delta = {'vpx-new': {'change': 'added'}, 'vpx-b': {'change': 'updated'},
                 'vpx-gone': {'change': 'removed'}}
        body = notes({'vpx-new': {'name': 'New'}, 'vpx-b': {'name': 'B'}}, delta)
        self.assertIn('## Newly added tables\n- [New](', body)
        self.assertIn('## Updated tables:\n- [B](', body)
        self.assertIn('## Removed tables\n- `vpx-gone`', body)
        self.assertEqual(notes({}, delta, []), '')


class GitTests(unittest.TestCase):
    """config_bundle and mirror_tree against a real repository."""

    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.cwd = os.getcwd()
        os.chdir(self.dir.name)
        self.addCleanup(os.chdir, self.cwd)
        self.env = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t',
                        GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t')
        os.environ.update({k: v for k, v in self.env.items() if k.startswith('GIT_')})
        self.addCleanup(lambda: [os.environ.pop(k, None) for k in
                                 ('GIT_AUTHOR_NAME', 'GIT_AUTHOR_EMAIL',
                                  'GIT_COMMITTER_NAME', 'GIT_COMMITTER_EMAIL')])
        subprocess.run(['git', 'init', '-q'], check=True)
        for path, text in {'tables/vpx-a/table.yml': 'a', 'tables/vpx-a/README.md': 'r',
                           'tables/vpx-b/table.yml': 'b', 'boxart/vpx-a.webp': 'art'}.items():
            Path(path).parent.mkdir(parents=True, exist_ok=True)
            Path(path).write_text(text)
        subprocess.run(['git', 'add', '-A'], check=True)
        subprocess.run(['git', 'commit', '-qm', 'x'], check=True)

    def test_trees_and_fingerprint_ignore_presentation(self):
        trees = config_bundle.folder_trees()
        self.assertEqual(set(trees), {'vpx-a', 'vpx-b'})
        before = config_bundle.fingerprint('tables/vpx-a')
        Path('tables/vpx-a/README.md').write_text('changed')
        subprocess.run(['git', 'commit', '-qam', 'readme'], check=True)
        self.assertEqual(config_bundle.fingerprint('tables/vpx-a'), before)
        self.assertNotEqual(config_bundle.folder_trees()['vpx-a'], trees['vpx-a'])

    def test_tree_builder_reuses_blobs_and_overlays(self):
        tree = mirror_tree.TreeBuilder('HEAD')
        tree.put('manifest.json', b'{}')
        tree.drop('tables/vpx-b')
        self.assertFalse(tree.take('HEAD', 'nope'))
        commit = tree.commit('m')
        files = mirror_tree.git('ls-tree', '-r', '--name-only', commit).splitlines()
        self.assertEqual(sorted(files), ['boxart/vpx-a.webp', 'manifest.json',
                                         'tables/vpx-a/README.md', 'tables/vpx-a/table.yml'])
        self.assertEqual(mirror_tree.show(commit, 'boxart/vpx-a.webp'), b'art')
        self.assertEqual(mirror_tree.git('rev-list', '--count', commit), '1')  # orphan


if __name__ == '__main__':
    unittest.main()
