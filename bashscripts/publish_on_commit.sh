#!/usr/bin/env sh

set -x

git diff --cached --name-only --diff-filter=AM | grep '.org$' | while IFS= read -r org_file; do
    emacs --batch \
        --eval "(progn
                (require 'org)
                (insert \"\n#+OPTIONS: \\n:t ^:nil \n\")
                (find-file \"$org_file\")
                (org-html-export-to-html))"

    git add "${org_file%.org}.html"

done
