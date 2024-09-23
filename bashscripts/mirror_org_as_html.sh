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

    # Create symbolic links for everything in the Org file's directory in the HTML folder,
    # except for the Org file itself (which we will copy later)
    find "$org_file_dir" -maxdepth 1 -mindepth 1 | while IFS= read -r file; do
        base_file=$(basename "$file")
        if [[ "$base_file" != "$(basename "$org_file")" ]]; then
            relative_path=$(realpath --relative-to="$html_folder" "$git_root/$file")
            # Symlink other files
            ln -Ts "$relative_path" "$html_folder/$base_file"
        fi
    done

    # Copy the Org file to the HTML folder
    cp "$org_file" "$html_folder/"

    # Now run the emacs batch process on the copied Org file
    emacs --batch \
        --eval "(progn
                (require 'org)
                (find-file \"$html_folder/$(basename "$org_file")\")
                (insert \"#+OPTIONS: \\n:t ^:nil \n \")
                (org-html-export-to-html))"

    # Get the path to the generated HTML file
    html_file="${org_file%.org}.html"

    # Check if the HTML file was generated successfully
    if [[ ! -f "$html_folder/$(basename "$html_file")" ]]; then
        echo "Error: HTML file not generated: $html_file"
        exit 1
    fi

    # Remove the copied Org file
    # rm "$html_folder/$(basename "$org_file")"

    git add --all "$html_folder"

done
