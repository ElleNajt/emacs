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
    orgnotebook="$1"
    # Extract the filename without extension
    base_name="${orgnotebook%.ipynb}"
    ipynb_file="${base_name}.ipynb"
    org_file="${base_name}.org"

    if [[ FORCE -eq 1 ]] || ! [ -f "$org_file" ]; then

        if [[ "$(uname -s)" == "Darwin" ]]; then
            # macOS requires an empty string argument with -i for in-place editing without backup
            sed -i '' "s/#+begin_src python/#+begin_src jupyter-python :exports both/gI" "$org_file" |\
                pandoc -f org -o "$ipynb_file" -
        else
            # Linux and other Unix-like systems
            sed -i "s/#+begin_src python/#+begin_src jupyter-python :exports both/gI" "$org_file" |\
                pandoc -f org -o "$ipynb_file" -
        fi

    else
        echo "Error: The file $ipynb_file already exists. Delete it first."
    fi

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


