#!/usr/bin/env sh
shopt -s dotglob
shopt -s nullglob

find . -type f -regex '.*\.\(py\|sh\|c\|cpp\|js\|java\|rs\|toml\|mi\|md\|txt\|json\|yaml\|yml\|nix\|env\|ini\|conf\|xml\|sql\|log\|lock\|el\)' -o -name 'Makefile' -o -name 'Dockerfile' -o -name '.gitignore' -o -name '.gitattributes' | while read -r file; do
    relpath=$(realpath --relative-to="$(pwd)" "$file") # Get the relative path
    echo "===== $relpath ====="
    cat "$file"
    echo -e "\n----------------------------------------------------"
done
