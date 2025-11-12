import json
import re
import sys
import yaml

import vpsdb

from pathlib import Path

def check_bundled(meta):
    """Checks if bundled fields have associated notes.

    Args:
      meta: The table metadata.

    Returns:
      None
    """
    print("Checking bundled fields...")
    for table, table_meta in meta.items():
        if (
            "backglassBundled" in table_meta
            and table_meta["backglassBundled"] is not None
        ):
            if table_meta["backglassBundled"]:
                if (
                    "backglassNotes" not in table_meta
                    or table_meta["backglassNotes"] is None
                ):
                    print(
                        f"ERROR: backglassBundled is True but backglassNotes is not found in table: {table}"
                    )
                    sys.exit(1)

                if (
                    "backglassChecksum" not in table_meta
                    or table_meta["backglassChecksum"] is None
                ):
                    print(
                        f"ERROR: backglassBundled is True but backglassChecksum is not found in table: {table}"
                    )
                    sys.exit(1)

                if not isinstance(table_meta["backglassNotes"], str):
                    print(f"ERROR: backglassNotes is not a string in table: {table}")
                    sys.exit(1)

            if table_meta["coloredROMBundled"]:
                if (
                    "coloredROMNotes" not in table_meta
                    or table_meta["coloredROMNotes"] is None
                ):
                    print(
                        f"ERROR: coloredROMBundled is True but coloredROMNotes is not found in table: {table}"
                    )
                    sys.exit(1)

                if (
                    "coloredROMChecksum" not in table_meta
                    or table_meta["coloredROMChecksum"] is None
                ):
                    print(
                        f"ERROR: coloredROMBundled is True but coloredROMChecksum is not found in table: {table}"
                    )
                    sys.exit(1)

                if not isinstance(table_meta["coloredROMNotes"], str):
                    print(f"ERROR: coloredROMNotes is not a string in table: {table}")
                    sys.exit(1)

            if table_meta["romBundled"]:
                if "romNotes" not in table_meta or table_meta["romNotes"] is None:
                    print(
                        f"ERROR: romBundled is True but romNotes is not found in table: {table}"
                    )
                    sys.exit(1)

                if "romChecksum" not in table_meta or table_meta["romChecksum"] is None:
                    print(
                        f"ERROR: romBundled is True but romChecksum is not found in table: {table}"
                    )
                    sys.exit(1)

                if not isinstance(table_meta["romNotes"], str):
                    print(f"ERROR: romNotes is not a string in table: {table}")
                    sys.exit(1)

def check_checksums(meta):
    """Checks if the checksums are valid MD5 hashes.

    Args:
      meta: The table metadata.

    Returns:
      None
    """
    print("Checking checksums...")
    for table, table_meta in meta.items():
        if "tableChecksum" not in table_meta or table_meta["tableChecksum"] is None:
            print(f"ERROR: vpxChecksum field not found in table: {table}")
            sys.exit(1)

        if (
            "backglassFileUrl" in table_meta
            and table_meta["backglassFileUrl"] is not None
        ) and (
            "backglassChecksum" not in table_meta
            or table_meta["backglassChecksum"] is None
        ):
            print(f"ERROR: backglassChecksum field not found in table: {table}")
            sys.exit(1)

        if (
            "coloredROMFileUrl" in table_meta
            and table_meta["coloredROMFileUrl"] is not None
        ) and (
            "coloredROMChecksum" not in table_meta
            or table_meta["coloredROMChecksum"] is None
        ):
            print(f"ERROR: coloredROMChecksum field not found in table: {table}")
            sys.exit(1)

        if ("pupFileUrl" in table_meta and table_meta["pupFileUrl"] is not None) and (
            "pupChecksum" not in table_meta or table_meta["pupChecksum"] is None
        ):
            print(f"ERROR: pupChecksum field not found in table: {table}")
            sys.exit(1)

        if ("romFileUrl" in table_meta and table_meta["romFileUrl"] is not None) and (
            "romChecksum" not in table_meta or table_meta["romChecksum"] is None
        ):
            print(f"ERROR: romChecksum field not found in table: {table}")
            sys.exit(1)

        for checksum_type in [
            "backglassChecksum",
            "coloredROMChecksum",
            "pupChecksum",
            "romChecksum",
            "tableChecksum",
        ]:
            if checksum_type not in table_meta or table_meta[checksum_type] is None:
                # We checked if it's required above, so we can skip it here
                continue

            if not isinstance(table_meta[checksum_type], str):
                print(f"ERROR: {checksum_type} is not a string in table: {table}")
                sys.exit(1)

            if not is_md5_hash(table_meta[checksum_type]):
                print(
                    f"ERROR: checksum {table_meta[checksum_type]} for {checksum_type} is not a valid MD5 hash in table: {table}"
                )
                sys.exit(1)

def check_fixes(meta):
    """Checks if the applyFixes field is valid.

    Args:
      meta: The table metadata.

    Returns:
      None
    """
    print("Checking applyFixes...")
    allowed_fixes = [
        "bass",
    ]

    for table, table_meta in meta.items():
        if "applyFixes" in table_meta and table_meta["applyFixes"] is not None:
            if not isinstance(table_meta["applyFixes"], list):
                print(f"ERROR: applyFixes is not a list in table: {table}")
                sys.exit(1)

            for fix in table_meta["applyFixes"]:
                if fix not in allowed_fixes:
                    print(
                        f"ERROR: applyFixes contains an invalid fix '{fix}' in table: {table}"
                    )
                    print(f"Allowed fixes: {','.join(allowed_fixes)}")
                    sys.exit(1)

def check_fps(meta):
    """Checks if the fps field is an integer."

    Args:
      meta: The table metadata.

    Returns:
      None
    """
    print("Checking fps...")
    for table, table_meta in meta.items():
        if "fps" not in table_meta or table_meta["fps"] is None:
            print(f"ERROR: fps field not found in table: {table}")
            sys.exit(1)

        if not isinstance(table_meta["fps"], int):
            print(f"ERROR: fps is not an integer in table: {table}")
            sys.exit(1)

def check_overrides(meta):
    """Checks if the overrides have versions defined.

    Args:
      meta: The table metadata.

    Returns:
      None
    """
    print("Checking overrides...")

    if "romUrlOverride" in meta and meta["romUrlOverride"] is not None:
        if not isinstance(meta["romUrlOverride"], str):
            print(f"ERROR: romUrlOverride is not a string")
            sys.exit(1)

        if "romVPSId" in meta and meta["romVPSId"] is not None:
            print(f"ERROR: romVPSId is not allowed with romUrlOverride")
            sys.exit(1)

        if "romVersionOverride" not in meta or meta["romVersionOverride"] is None:
            print(
                f"ERROR: romUrlOverride defined and romVersionOverride field not found"
            )
            sys.exit(1)

def check_testers(meta):
    """Checks if the testers field is a list."

    Args:
      meta: The table metadata.

    Returns:
      None
    """
    print("Checking testers...")
    for table, table_meta in meta.items():
        if "testers" not in table_meta or table_meta["testers"] is None:
            print(f"ERROR: testers field not found in table: {table}")
            sys.exit(1)

        if not isinstance(table_meta["testers"], list):
            print(f"ERROR: testers is not a list in table: {table}")
            sys.exit(1)

def is_md5_hash(hash_string):
    """Checks if a string is a valid MD5 hash.

    Args:
      hash_string: The string to check.

    Returns:
      True if the string is a valid MD5 hash, False otherwise.
    """
    return bool(re.match(r"^[a-f0-9]{32}$", hash_string))


if __name__ == "__main__":
    # Accept optional file paths on the command line. If none are provided,
    # auto-discover all table.yml files under the external/ directory.
    files = sys.argv[1:]

    if files:
        # Keep only files that actually exist (skip deleted/missing paths)
        files = [f for f in files if Path(f).is_file()]
        if not files:
            print("No valid table.yml files passed on the command line. Nothing to validate.")
            sys.exit(0)
    else:
        base = Path("external")
        files = [str(p) for p in base.rglob("table.yml")]
        if not files:
            print("No table.yml files found under external/ — skipping validation.")
            sys.exit(0)

    # For each discovered file, perform YAML-level checks (check_overrides)
    for f in files:
        try:
            with open(f, "r") as table_data:
                table_yaml = yaml.safe_load(table_data)
        except Exception as e:
            print(f"ERROR: Failed to load {f}: {e}")
            sys.exit(1)

        path = Path(f)
        folder_name = path.parent.name
        print(f"Processing {folder_name} ({f})")

        # Perform checks on the YAML file content
        check_overrides(table_yaml)

    # Render metadata for all files in a single call, then run the meta-level checks
    meta = vpsdb.get_table_meta(files, warn_on_error=False)

    check_bundled(meta)
    check_checksums(meta)
    check_fixes(meta)
    check_fps(meta)
    check_testers(meta)

    j = json.dumps(meta, indent=4)
    print(j)
    sys.exit(0)