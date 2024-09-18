#!/usr/bin/env python3

import io


PANDAS_AVAILABLE = False
_original_repr = {}
_original_str = {}

try:
    import pandas as pd
    PANDAS_AVAILABLE = True
except ImportError:
    pass

def org_repr(obj):
    output = io.StringIO()
    if isinstance(obj, pd.DataFrame):
        obj.to_csv(output, sep="|",index = False)
    elif isinstance(obj, pd.Series):
        pd.DataFrame(obj).to_csv(output, sep="|", index=False, header=[obj.name or ''])
    output.seek(0)
    table = output.read().strip().split("\n")
    table = [f"| {line.strip()} |" for line in table]

    if len(table) > 1:
        header_width = len(table[0])
        hline = f"|{'-' * (header_width - 2)}|"
        table.insert(1, hline)
    return "\n".join(table)

def enable():
    global PANDAS_AVAILABLE
    if not PANDAS_AVAILABLE:
        print("Pandas is not available in this environment. Org-mode representation cannot be enabled.")
        return

    for obj in [pd.DataFrame, pd.Series]:
        if obj not in _original_repr:
            _original_repr[obj] = obj.__repr__
            _original_str[obj] = obj.__str__

        obj.__str__ = org_repr
        obj.__repr__ = org_repr

def disable():
    global PANDAS_AVAILABLE
    if not PANDAS_AVAILABLE:
        print("Pandas is not available in this environment. No changes to revert.")
        return

    for obj in [pd.DataFrame, pd.Series]:
        if obj in _original_repr:
            obj.__repr__ = _original_repr[obj]
            obj.__str__ = _original_str[obj]

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
    print(df.Name)

    # disable()
    # print(df)
