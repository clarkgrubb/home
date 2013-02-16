#!/bin/sh

PAGES='scripting scripting2 embeddable cpp c pascal lisp ml logic stack shell data numerical-analysis computer-algebra'

download_dir=$(mktemp -d download_pages_XXX)
echo downloading pages to $download_dir

for page in $PAGES
do
    PYTHONIOENCODING=utf-8 ./page_content.py $page > ${download_dir}/${page}.txt
done
