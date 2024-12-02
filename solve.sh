#!/bin/bash

# Default values
CURRENT_YEAR=$(date +%Y)
DEFAULT_LANG=${AOC_LANG:-"ocaml"}

year=${1:-$CURRENT_YEAR}
day=$2
lang=${3:-$DEFAULT_LANG}

if [ -z "$day" ]; then
    echo "Usage: $0 [year] day [language]"
    echo "Example: $0 2024 1 ocaml"
    exit 1
fi

case $lang in
    "ocaml")
        cd ocaml
        dune exec ./main.exe -- $year $day
        cd ..
        ;;
    "rust")
        cd rust
        cargo run $year $day
        cd ..
        ;;
    *)
        echo "Unsupported language: $lang"
        exit 1
        ;;
esac