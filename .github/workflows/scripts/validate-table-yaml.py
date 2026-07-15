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
    """Checks that required checksums are present.

    Requiredness depends on rendered fields (e.g. backglassFileUrl), so it is
    checked here on the vpsdb-rendered metadata. The string-vs-list format and
    MD5 validity of the raw table.yml values are checked by
    check_checksum_format instead, since vpsdb has already normalized every
    present checksum to a list by this point.

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

        additional_roms = table_meta.get("additionalRoms")
        if additional_roms:
            for i, rom in enumerate(additional_roms):
                if not rom.get("checksum"):
                    print(
                        f"ERROR: additionalRoms[{i}] missing checksum in table: {table}"
                    )
                    sys.exit(1)


# Raw table.yml checksum keys. vpsdb renames vpxChecksum -> tableChecksum, so
# these are the names as authored in table.yml (checked before rendering).
CHECKSUM_YAML_KEYS = [
    "altSoundChecksum",
    "backglassChecksum",
    "coloredROMChecksum",
    "diffChecksum",
    "pupChecksum",
    "romChecksum",
    "vpxChecksum",
]


def validate_checksum_value(label, value):
    """Validate one checksum value (one or more acceptable MD5 hashes):
      - a single hash MUST be a plain string, and
      - two or more hashes MUST be a list (a single-element list is rejected).
    Every hash must be a valid MD5. Exits on any violation.

    Args:
      label: Human-readable field name used in error messages.
      value: The raw checksum value (string or list of strings).

    Returns:
      None
    """
    if isinstance(value, str):
        hashes = [value]
    elif isinstance(value, list):
        if len(value) < 2:
            print(f"ERROR: {label} has a single entry; use a string instead of a list")
            sys.exit(1)
        hashes = value
    else:
        print(f"ERROR: {label} must be a string or a list of strings")
        sys.exit(1)

    for checksum in hashes:
        if not isinstance(checksum, str) or not is_md5_hash(checksum.lower()):
            print(f"ERROR: checksum {checksum} for {label} is not a valid MD5 hash")
            sys.exit(1)


def check_checksum_format(meta):
    """Validates the format of raw table.yml checksum fields.

    vpsdb normalizes both string and list forms to a list for the manifest, so
    this format rule only governs how table.yml is authored (see
    validate_checksum_value for the rule).

    Args:
      meta: The raw table.yml content (a single table's parsed dict).

    Returns:
      None
    """
    if not isinstance(meta, dict):
        return

    for key in CHECKSUM_YAML_KEYS:
        value = meta.get(key)
        if value is None:
            continue
        validate_checksum_value(key, value)


def check_additional_roms(meta):
    """Validates the raw table.yml additionalRoms field (a list of ROM objects).

    Each entry mirrors the primary ROM: checksum required (string or list of
    MD5s); vpsId xor urlOverride (mutually exclusive); urlOverride requires
    versionOverride; bundled (bool) requires notes;
    vpsId/urlOverride/versionOverride/notes must be strings.

    Args:
      meta: The raw table.yml content (a single table's parsed dict).

    Returns:
      None
    """
    if not isinstance(meta, dict):
        return

    value = meta.get("additionalRoms")
    if value is None:
        return
    if isinstance(value, dict):
        value = [value]
    if not isinstance(value, list):
        print("ERROR: additionalRoms must be a list of ROM objects")
        sys.exit(1)

    for i, entry in enumerate(value):
        label = f"additionalRoms[{i}]"
        if not isinstance(entry, dict):
            print(f"ERROR: {label} must be a mapping")
            sys.exit(1)

        if entry.get("checksum") is None:
            print(f"ERROR: {label} is missing the required checksum field")
            sys.exit(1)
        validate_checksum_value(f"{label}.checksum", entry.get("checksum"))

        for field in ("vpsId", "urlOverride", "versionOverride", "notes"):
            if entry.get(field) is not None and not isinstance(entry.get(field), str):
                print(f"ERROR: {label}.{field} must be a string")
                sys.exit(1)

        if "bundled" in entry and not isinstance(entry.get("bundled"), bool):
            print(f"ERROR: {label}.bundled must be a boolean")
            sys.exit(1)

        # A bundled ROM ships in the table download, so authors must document it
        # (mirrors the primary ROM's romBundled -> romNotes rule).
        if entry.get("bundled") and entry.get("notes") is None:
            print(f"ERROR: {label} is bundled but has no notes")
            sys.exit(1)

        vps_id = entry.get("vpsId")
        url_override = entry.get("urlOverride")
        if vps_id is not None and url_override is not None:
            print(f"ERROR: {label} has both vpsId and urlOverride (mutually exclusive)")
            sys.exit(1)
        if url_override is not None and entry.get("versionOverride") is None:
            print(f"ERROR: {label} has urlOverride but no versionOverride")
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
        check_checksum_format(table_yaml)
        check_additional_roms(table_yaml)

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