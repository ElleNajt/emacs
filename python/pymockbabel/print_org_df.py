#!/usr/bin/env python3

import io

PANDAS_AVAILABLE = False
_print_org_df_original_repr = {}
_print_org_df_original_str = {}

try:
    import pandas as pd
    PANDAS_AVAILABLE = True
except ImportError:
    pass

def org_repr(df):
    output = io.StringIO()
    df.to_csv(output, sep="|", index=isinstance(df, pd.Series))
    output.seek(0)
    table = output.read().strip().split("\n")
    table = [f"| {line.strip()} |" for line in table]
    return "\n".join(table)

def enable():
    global PANDAS_AVAILABLE
    if not PANDAS_AVAILABLE:
        print("Pandas is not available in this environment. Org-mode representation cannot be enabled.")
        return

    for obj in [pd.DataFrame, pd.Series]:
        if obj not in _print_org_df_original_repr:
            _print_org_df_original_repr[obj] = obj.__repr__
            _print_org_df_original_str[obj] = obj.__str__

        obj.__str__ = org_repr
        obj.__repr__ = org_repr

def disable():
    global PANDAS_AVAILABLE
    if not PANDAS_AVAILABLE:
        print("Pandas is not available in this environment. No changes to revert.")
        return

    for obj in [pd.DataFrame, pd.Series]:
        if obj in _print_org_df_original_repr:
            obj.__repr__ = _print_org_df_original_repr[obj]
            obj.__str__ = _print_org_df_original_str[obj]

def is_enabled():
    return PANDAS_AVAILABLE and pd.DataFrame.__repr__ == org_repr

def is_pandas_available():
    return PANDAS_AVAILABLE

if __name__ == "__main__":
    import pandas as pd
    import numpy as np

    data = {
        'Name': ['Alice', 'Bob', 'Charlie', 'David', 'Eva'],
        'Age': [25, 30, 35, 28, 22],
        'City': ['New York', 'San Francisco', 'London', 'Paris', 'Tokyo'],
        'Score': [92.5, 88.0, 95.2, 78.9, 90.1]
    }
    df = pd.DataFrame(data)
    enable()

    print(df)

    disable()
    print(df)
