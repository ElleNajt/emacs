#!/usr/bin/env sh

set -x

echo "$(env | grep PATH)"

git diff --cached --name-only --diff-filter=AM | grep '.org$' | while IFS= read -r org_file; do
    echo "Processing: $org_file"
    # base_name=$(basename "${org_file}")
    org_file_dir=$(dirname "${org_file}")
    # echo "$(pwd)"
    # cd "$git_root" || exit
    git_root=$(git rev-parse --show-toplevel)
    relative_path=$(realpath --relative-to="$git_root" "$org_file_dir")
    html_folder="${git_root}/html/${relative_path}"

    [[ -d "$html_folder" ]] || mkdir -p "$html_folder"

    # Use Emacs in batch mode to export the Org file to HTML
    emacs --batch \
        --eval "(progn
                (require 'org)
                (find-file \"$org_file\")
                (org-html-export-to-html))"
    # export-org-to-html "${org_file}"

    html_file="${org_file%.org}.html"
    mv "$html_file" "$html_folder"
    cp "${dirname}/*" "${html_folder}"
    git add "html/${html_file}"

done
