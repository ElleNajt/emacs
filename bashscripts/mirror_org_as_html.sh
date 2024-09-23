#!/usr/bin/env sh

set -x

git diff --cached --name-only --diff-filter=AM | grep '.org$' | while IFS= read -r org_file; do
    echo "Processing: $org_file"
    base_name=$(basename "${org_file}")
    base_name="${base_name%.org}"
    file_directory=$(dirname "${org_file}")
    htmls_folder_name="auto_generated_htmls"
    htmls_dir="${file_directory}/${htmls_folder_name}"
    html_file="${htmls_dir}/${base_name}.html"
    export-org-to-html-recursively ${org_file}
    git add "${html_file}"
done
