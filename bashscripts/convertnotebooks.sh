#!/usr/bin/env zsh
#
set -x
set -euo pipefail
RECURSIVE=0
FORCE=0
while getopts "hrf" flag; do
 case $flag in
   h) # Handle the -h flag
   echo "-f to force overwrite, -r to apply script recursively"
   ;;
   r) # Handle the -r flag
   RECURSIVE=1
   # Enable recursive mode -- all files in all subdirectories
   ;;
   f) # Handle the -force overwrite flag
   FORCE=1
   ;;
   \?)
   echo "Invalid option."
   # Handle invalid options
   ;;
 esac
done


currentdir=$(dirname "$0")
text_block_file="${currentdir}/$(hostname)-default-python-env.txt"

# Check if the text block file exists
if ! [ -f "$text_block_file" ]; then
    echo "Error: The file $text_block_file does not exist."
    exit 1
fi

setopt extendedglob

convert_file() {
    notebook="$1"
    # Extract the filename without extension
    base_name="${notebook%.ipynb}"
    org_file="${base_name}.org"

    # Convert .ipynb to .org using pandoc
    if [[ FORCE -eq 1 ]] || ! [ -f "$org_file" ]; then
        pandoc "$notebook" -o "$org_file"
    else
        echo "Error: The file $org_file already exists. Delete it first."
    fi
    file_name_only=$(basename -- "$base_name")
    echo "$file_name_only"
    python_header="$(cat "${text_block_file}" | sed "s/SESSION_NAME_PLACEHOLDER/${file_name_only}/")"
    # another way to handle this, replacing the /s with --
    # python_header="$(cat "${text_block_file}" | sed "s/SESSION_NAME_PLACEHOLDER/${base_name//\//--}/")"

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
}

if [[ $RECURSIVE -eq 1 ]]; then
    for notebook in **/*.ipynb; do
        convert_file "$notebook"
    done
else
    for notebook in *.ipynb; do
        convert_file "$notebook"
    done
fi


# Loop through all .ipynb files in the current directory
