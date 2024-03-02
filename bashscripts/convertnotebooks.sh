#!/bin/bash

set -euo pipefail

currentdir=$(dirname "$0")
text_block_file="${currentdir}/$(hostname)-default-python-env.txt"

# Check if the text block file exists
if ! [ -f "$text_block_file" ]; then
    echo "Error: The file $text_block_file does not exist."
    exit 1
fi

# Loop through all .ipynb files in the current directory
for notebook in *.ipynb; do
    # Extract the filename without extension
    base_name="${notebook%.ipynb}"
    org_file="${base_name}.org"

    # Convert .ipynb to .org using pandoc
    if ! [ -f "$org_file" ]; then
        pandoc "$notebook" -o "$org_file"
    else
        echo "Error: The file $org_file already exists. Delete it first."
    fi

    python_header="$(cat "${text_block_file}" | sed "s/SESSION_NAME_PLACEHOLDER/${base_name}/")"

    # Check operating system
    if [[ "$(uname -s)" == "Darwin" ]]; then
        # macOS requires an empty string argument with -i for in-place editing without backup
        sed -i '' 's/jupyter-python/python/' "$org_file"
    else
        # Linux and other Unix-like systems
        sed -i 's/jupyter-python/python/' "$org_file"
    fi

    # Add the fixed text block to the top of the .org file
    temp_file=$(mktemp)
    mv "$org_file" "$temp_file"

    # Prepend the text block to the .org file
    echo "$python_header" > "$org_file"
    cat "$temp_file" >> "$org_file"

    # Clean up the temporary file
    rm "$temp_file"
done
