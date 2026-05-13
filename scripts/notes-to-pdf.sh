#!/usr/bin/env bash

# Converts Markdown notes in ./notes to PDF.
# Run from the root direcotry: scripts/to-pdf.sh

for file in notes/*.md
do 
    NEW_NAME="${file/.md/.pdf}"
    echo "pandoc -o $NEW_NAME $file"
    pandoc -o $NEW_NAME $file
done
