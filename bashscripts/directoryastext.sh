#!/usr/bin/env sh

shopt -s dotglob
shopt -s nullglob

for file in *; do
    if [ -f "$file" ]; then # Ensure it's a file
        echo "===== $file ====="
        cat "$file"
        echo -e "\n----------------------------------------------------"
    fi
done
