#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
source_dir="$project_dir/www/species"
output_dir="$project_dir/docs/assets/plants"

mkdir -p "$output_dir"

find "$source_dir" -maxdepth 1 -type f \( -iname '*.jpg' -o -iname '*.jpeg' \) -print0 |
  while IFS= read -r -d '' source_file; do
    filename=$(basename "$source_file")
    stem=${filename%.*}
    sips -Z 720 -s format jpeg -s formatOptions 60 "$source_file" --out "$output_dir/$stem.jpg" >/dev/null
  done

printf 'Prepared plant images in %s\n' "$output_dir"
