#!/usr/bin/env sh
shopt -s dotglob
shopt -s nullglob

find . -type f | while read -r file; do
    relpath=$(realpath --relative-to="$(pwd)" "$file") # Get the relative path
    echo "===== $relpath ====="
    cat "$file"
    echo -e "\n----------------------------------------------------"
done
