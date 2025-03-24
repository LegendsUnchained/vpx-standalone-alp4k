import configparser
import sys


def check_backbuffer_scale(config):
    backbuffer_scale = config.get("Standalone", "BackBufferScale")

    allowed_values = [".833333", ".666666", ".5", ".416666", ".355729", ".333333"]
    if backbuffer_scale not in allowed_values:
        print(f"Invalid BackBufferScale value: {backbuffer_scale}")
        sys.exit(1)


if __name__ == "__main__":
    table_ini = sys.argv[1]
    config = configparser.ConfigParser()
    config.read(table_ini)

    # Run checks
    check_backbuffer_scale(config)
