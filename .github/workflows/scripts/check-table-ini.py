import configparser
import sys


def check_backbuffer_scale(config):
    if not config.has_option('Standalone', 'BackBufferScale'):
        return

    backbuffer_scale = config.get("Standalone", "BackBufferScale")

    allowed_values = ["1.0", "0.833333", "0.666666", "0.5", "0.416666", "0.355729", "0.333333"]
    if backbuffer_scale not in allowed_values:
        print(f"Invalid BackBufferScale value: {backbuffer_scale}")
        print(f"Allowed values: {', '.join(allowed_values)}")
        sys.exit(1)


if __name__ == "__main__":
    table_ini = sys.argv[1]
    
    config = configparser.ConfigParser()
    config.read(table_ini)

    # Run checks
    check_backbuffer_scale(config)
