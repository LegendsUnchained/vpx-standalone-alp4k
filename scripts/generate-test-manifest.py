#!/usr/bin/env python3
"""Build a local test bundle for the Manifest Tester: the rendered manifest plus
the table's config zip (table.ini, launcher config, …).

Run from the repo root:

    python scripts/generate-test-manifest.py vpx-starwars

Writes into the (gitignored) manifest-tester/ folder at the repo root:
    manifest.json     — generate-manifest.py output for the single table (paste
                        this into the Manifest Tester).
    <tablename>.zip   — a zip of external/<tablename>/'s contents, i.e. the same
                        config bundle a release publishes and the wizard installer
                        extracts (upload this in the tester's config slot).

Reuses .github/workflows/scripts/generate-manifest.py for the manifest so both
stay in lockstep.
"""
import argparse
import importlib.util
import json
import shutil
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
# generate-manifest.py (and the vpsdb library it imports) live with the workflow
# scripts; put that dir on sys.path so the module's `import vpsdb` resolves.
WORKFLOW_SCRIPTS = REPO_ROOT / ".github" / "workflows" / "scripts"

# Fixed, gitignored scratch folder at the repo root (cwd), so output never needs
# to be specified and never lands in a commit.
OUTPUT_DIR = Path("manifest-tester")


def load_generate_manifest():
    """Load generate-manifest.py by path (its hyphenated name isn't importable),
    with its own directory on sys.path so its `import vpsdb` works."""
    sys.path.insert(0, str(WORKFLOW_SCRIPTS))
    path = WORKFLOW_SCRIPTS / "generate-manifest.py"
    spec = importlib.util.spec_from_file_location("generate_manifest", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Build manifest.json + config zip for one table into manifest-tester/.",
    )
    parser.add_argument("table", help="table folder name, e.g. vpx-starwars")
    args = parser.parse_args(argv)

    gm = load_generate_manifest()

    files = gm.resolve_files([args.table])
    if not files:
        print(f"ERROR: no table.yml found for '{args.table}'", file=sys.stderr)
        return 1
    if len(files) > 1:
        print(
            f"ERROR: expected a single table, but '{args.table}' matched {len(files)}",
            file=sys.stderr,
        )
        return 1

    table_dir = Path(files[0]).parent
    table_name = table_dir.name

    # The config zip must carry the GL launcher image; without it the install
    # fails at createGLStructure ("GL image is required"). Fail before writing
    # anything rather than producing a config zip that can't install.
    required_gl_files = ["launcher.png"]
    missing = [name for name in required_gl_files if not (table_dir / name).is_file()]
    if missing:
        print(
            f"ERROR: {table_name} is missing required GL structure file(s): "
            f"{', '.join(missing)} — cannot build a valid config zip",
            file=sys.stderr,
        )
        return 1

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # 1) manifest.json — the exact rendering the tester expects. strict=True so an
    #    unresolved VPS id fails loudly here rather than producing a partial file.
    tables = gm.render(files, strict=True)
    manifest_path = OUTPUT_DIR / "manifest.json"
    manifest_path.write_text(json.dumps(tables, indent=2) + "\n")

    # 2) <tablename>.zip — the CONTENTS of external/<tablename>/ (matches the
    #    release bundle that ExtractRepoConfig downloads; the installer routes the
    #    config files, e.g. table.ini/launcher.png, by basename).
    zip_base = OUTPUT_DIR / table_name
    shutil.make_archive(str(zip_base), "zip", root_dir=str(table_dir))
    zip_path = zip_base.with_suffix(".zip")

    print(f"Wrote {manifest_path}")
    print(f"Wrote {zip_path}")
    print(
        f"\nIn the Manifest Tester: paste {manifest_path.name}, then upload "
        f"{zip_path.name} in the config slot before installing."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
