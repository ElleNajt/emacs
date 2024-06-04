#!/usr/bin/env sh

for file in $(git diff --cached --name-only --diff-filter=D | grep '.org$')
do
    base_name="${file%.org}"
    ipynb_file="${base_name}_githookgenerated.ipynb"
    rm -f "$ipynb_file"
    git rm --cached "$ipynb_file"
done

for file in $(git diff --cached --name-only --diff-filter=AM | grep '.org$')
do
    base_name="${file%.org}"
    org_file="${base_name}.org"
    file_directory=$(dirname "${org_file}")
    ipynb_file="${base_name}_githookgenerated.ipynb"
    sed -i '' "s/#+begin_src python/#+begin_src jupyter-python :exports both/gI" "$org_file" |\
            pandoc --resource_path="$file_directory" -f org -o "$ipynb_file" -
    git add "${ipynb_file}"
done
