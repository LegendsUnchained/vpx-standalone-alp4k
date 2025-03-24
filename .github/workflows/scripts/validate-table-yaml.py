import json
import sys

import vpsdb

if __name__ == "__main__":
    table_yaml = sys.argv[1]
    meta = vpsdb.get_table_meta([table_yaml], warn_on_error=False)
    j = json.dumps(meta, indent=4)
    print(j)
    sys.exit(0)

