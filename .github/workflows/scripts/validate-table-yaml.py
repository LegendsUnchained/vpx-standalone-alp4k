import json
import re
import sys

import vpsdb


def check_checksums(meta):
    """Checks if the checksums are valid MD5 hashes.

    Args:
      meta: The table metadata.

    Returns:
      None
    """
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
    for table, table_meta in meta.items():
        if "fps" not in table_meta or table_meta["fps"] is None:
            print(f"ERROR: fps field not found in table: {table}")
            sys.exit(1)

        if not isinstance(table_meta["fps"], int):
            print(f"ERROR: fps is not an integer in table: {table}")
            sys.exit(1)


def check_testers(meta):
    """Checks if the testers field is a list."

    Args:
      meta: The table metadata.

    Returns:
      None
    """
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
    table_yaml = sys.argv[1]
    meta = vpsdb.get_table_meta([table_yaml], warn_on_error=False)

    # Perform checks
    check_fixes(meta)
    check_fps(meta)
    check_checksums(meta)
    check_testers(meta)

    j = json.dumps(meta, indent=4)
    print(j)
    sys.exit(0)
