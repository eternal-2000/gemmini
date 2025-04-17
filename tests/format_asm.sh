#!/bin/bash

# Formats assembly file for readability and outputs to new file
# Does not delete original assembly file

if [ $# -ne 2 ]; then
    echo "Requires two arguments: $0 <input.s> <output.s>"
    exit 1
fi

input=$1
output=$2

# Add new lines after labels
awk '/:$/ {print $0 "\n"; next} {print}' "$input" > "$output"
