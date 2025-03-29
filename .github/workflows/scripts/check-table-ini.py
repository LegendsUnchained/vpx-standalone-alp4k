import configparser
import sys


def check_backbuffer_scale(config):
    if not config.has_option("Standalone", "BackBufferScale"):
        return

    backbuffer_scale = config.get("Standalone", "BackBufferScale")

    allowed_values = [
        "1.0",
        "0.833333",
        "0.666666",
        "0.5",
        "0.416666",
        "0.355729",
        "0.333333",
    ]
    if backbuffer_scale not in allowed_values:
        print(f"Invalid BackBufferScale value: {backbuffer_scale}")
        print(f"Allowed values: {', '.join(allowed_values)}")
        sys.exit(1)


def check_duplicate_settings(config, default_config):
    for section in config.sections():
        if section in default_config.sections():
            for option in config[section]:
                if (
                    option in default_config[section]
                    and config[section][option] == default_config[section][option]
                ):
                    print("Duplicate setting found in Default_VPinballX.ini:")
                    print(f"[{section}]")
                    print(f"{option}")
                    sys.exit(1)


def check_sound3d(config):
    if config.has_option("Player", "Sound3D"):
        print(f"Sound3D should not be defined in table.ini")
        sys.exit(1)


if __name__ == "__main__":
    table_ini = sys.argv[1]
    default_ini = sys.argv[2]

    config = configparser.ConfigParser()
    config.read(table_ini)

    default_config = configparser.ConfigParser()
    default_config.read(default_ini)

    # Run checks
    check_backbuffer_scale(config)
    # check_duplicate_settings(config, default_config)
    check_sound3d(config)
