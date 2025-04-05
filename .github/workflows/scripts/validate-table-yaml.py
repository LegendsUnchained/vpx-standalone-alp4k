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
        for checksum_type in [
            "backglassChecksum",
            "coloredROMChecksum",
            "romChecksum",
            "vpxChecksum",
        ]:
            if checksum_type not in table_meta or table_meta[checksum_type] is None:
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
            print(
                f"ERROR: romVPSId is not allowed with romUrlOverride"
            )
            sys.exit(1)

        if (
            "romVersionOverride" not in meta
            or meta["romVersionOverride"] is None
        ):
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
    yml_file = sys.argv[1]

    with open(yml_file, "r") as table_data:
        table_yaml = yaml.safe_load(table_data)

    path = Path(yml_file)
    folder_name = path.parent.name
    print(f"Processing {folder_name}")

    # Perform checks on the YAML file
    check_overrides(table_yaml)

    # Perform checks on the rendered metadata
    meta = vpsdb.get_table_meta([yml_file], warn_on_error=False)

    check_bundled(meta)
    check_checksums(meta)
    check_fixes(meta)
    check_fps(meta)
    check_testers(meta)

    j = json.dumps(meta, indent=4)
    print(j)
    sys.exit(0)
