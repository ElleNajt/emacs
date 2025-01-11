#!/usr/bin/env zsh

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

setopt extendedglob

convert_file() {
    file="$1"
    base_name="${file%.odt}"
    odt_file="${base_name}.odt"
    org_file="${base_name}.org"

    if [[ FORCE -eq 1 ]] || ! [ -f "$org_file" ]; then
        pandoc --extract-media=./images -f odt "$odt_file" -o "$org_file"
    else
        echo "Error: The file $org_file already exists. Delete it first."
    fi

}

if [[ $RECURSIVE -eq 1 ]]; then
    for file in **/*.odt; do
        convert_file "$file"
    done
else
    for file in *.odt; do
        convert_file "$file"
    done
fi
