#!/usr/bin/env python3
import io

def orgprint(df):
    # maybe expand this to org print other types in the future
    output = io.StringIO()
    df.to_csv(output, sep="|", index = False)
    output.seek(0)
    table = output.read()
    table = table.split("\n")[:-1]
    table = [f" | {line.strip()} | " for line in table]
    print ("\n".join(table))


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

    orgprint(df)
