#!/bin/bash

# Default values
CURRENT_YEAR=$(date +%Y)
DEFAULT_LANG=${AOC_LANG:-"ocaml"}

year=${1:-$CURRENT_YEAR}
day=$2
part=$3
lang=${4:-$DEFAULT_LANG}

if [ -z "$day" ] || [ -z "$part" ]; then
    echo "Usage: $0 [year] day part [language]"
    echo "Example: $0 2023 1 1 python"
    exit 1
fi

day_padded=$(printf "%02d" $day)
solution_file="$year/$lang/day_${day_padded}_${part}.$lang"

if [ ! -f "$solution_file" ]; then
    echo "Solution file not found: $solution_file"
    exit 1
fi

case $lang in
    "ocaml")
        cd ocaml
        dune exec ./2023/day_${day_padded}_${part}.exe
        cd ..
        ;;
    "rust")
        cd rust
        cargo run --bin day_${day_padded}_${part}
        cd ..
        ;;
    "python")
        python3 python/2023/day_${day_padded}_${part}.py
        ;;
    *)
        echo "Unsupported language: $lang"
        exit 1
        ;;
esac