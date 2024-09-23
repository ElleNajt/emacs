#!/usr/bin/env sh

set -x

echo "$(env | grep PATH)"

# Loop through each staged .org file
git diff --cached --name-only --diff-filter=AM | grep '.org$' | while IFS= read -r org_file; do
    org_file_dir=$(dirname "${org_file}")
    git_root=$(git rev-parse --show-toplevel)
    relative_path=$(realpath --relative-to="$git_root" "$org_file_dir")
    html_folder="${git_root}/html/${relative_path}"
    [[ -d "$html_folder" ]] || mkdir -p "$html_folder"

    emacs --batch \
        --eval "(progn
                (require 'org)
                (find-file \"$org_file\")
                (org-html-export-to-html))"

    html_file="${org_file%.org}.html"

    if [[ ! -f "$html_file" ]]; then
        echo "Error: HTML file not generated: $html_file"
        exit
    fi

    mv "$html_file" "$html_folder"

    # Create symbolic links for everything in the Org file's directory in the HTML folder
    # this is so that images and so on render
    find "$org_file_dir" -maxdepth 1 -mindepth 1 | while IFS= read -r file; do
        base_file=$(basename "$file")
        relative_path=$(realpath --relative-to="$html_folder" "$git_root/$file")
        # shenanigans with relative paths to get symlinks to work
        ln -Ts "$relative_path" "$html_folder/$base_file"
    done

    git add --all "$html_folder"

done
