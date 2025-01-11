#!/usr/bin/env sh

set -x

# Find the root of the git repository
repo_root=$(git rev-parse --show-toplevel)

SET_DIR=0
publishing_dir=""

# Define a temporary file to hold the publishing directory path
tmp_file=$(mktemp)
# Process each staged .org file
git diff --cached --name-only --diff-filter=AM | grep '\.org$' | while IFS= read -r org_file; do

    # Check if it's a regular file and not a directory
    if [ -f "$org_file" ]; then
        emacs --batch \
            --eval "(progn
                    (require 'org)
                    (require 'ox-publish)
                    (load-file (expand-file-name \"publication_config.el\" \"$repo_root\"))
                    (find-file \"$org_file\")
                    (org-publish-current-file t))"

        # Only find the publishing directory if SET_DIR is 0
        if [ "$SET_DIR" -eq 0 ]; then

            # Extract the publishing directory dynamically (no hardcoded project name)
            emacs --batch \
                --eval "(progn
                        (require 'org)
                        (load-file (expand-file-name \"publication_config.el\" \"$repo_root\"))
                        (find-file \"$org_file\")
                        ;; Get the first project (dynamic retrieval)
                        (let* ((first-project (car org-publish-project-alist))
                            (project-settings (cdr first-project)))
                        ;; Get the publishing directory from the first project
                        (let ((publishing-dir (plist-get project-settings :publishing-directory)))
                            (if publishing-dir
                                (with-temp-file \"$tmp_file\"
                                (insert (expand-file-name publishing-dir)))
                            (error \"Publishing directory not found\")))))"

            # Set SET_DIR to 1 to prevent repeating this step
            SET_DIR=1
        fi
    fi
done

# Read the publishing directory from the temp file (if it exists)
publishing_dir=$(cat "$tmp_file" 2>/dev/null)

# Clean up the temp file
rm "$tmp_file"

# If the publishing directory was found, run git add --all
if [ -n "$publishing_dir" ]; then
    git add --all "$publishing_dir"
else
    echo "Publishing directory not set or found."
fi
