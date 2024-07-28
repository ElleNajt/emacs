#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.pandas python3Packages.seaborn python3Packages.matplotlib

import sys

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

import pymockbabel


# Example pandas and seaborn plot code
def create_pandas_plot():
    df = pd.DataFrame({"x": [1, 2, 3, 4, 5], "y": [10, 11, 12, 13, 14]})
    df.plot(x="x", y="y", kind="line")
    plt.show()


def create_seaborn_plot():
    df = sns.load_dataset("iris")
    sns.scatterplot(data=df, x="sepal_length", y="sepal_width", hue="species")
    plt.show()


def main():
    print("here")
    outputs_and_file_paths, output_types, list_writer = pymockbabel.setup("basic_test")

    print("Starting the plotting functions...")

    create_pandas_plot()
    create_seaborn_plot()
    print("Plotting completed successfully.")

    try:
        print(1 / 0)
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)

    pymockbabel.display(outputs_and_file_paths, output_types, list_writer)


if __name__ == "__main__":
    main()
