#!/usr/bin/env zsh

# Check if pattern argument is provided
if [[ $# -ne 1 ]]; then
  echo "Error: Please provide a globbing pattern as argument" >&2
  exit 1
fi

pattern="$1"

# Find all matching .r and .R files (case insensitive)
r_files=(${~pattern})

# Check if any R files exist
if [[ ${#r_files[@]} -eq 0 ]]; then
  echo "Error: No files matching pattern '$pattern'" >&2
  exit 1
fi

# Validate that all filenames start with a number
for file in "${r_files[@]}"; do
  basename=$(basename "$file")
  if [[ ! "$basename" =~ ^[0-9] ]]; then
    echo "Error: File '$basename' does not start with a number" >&2
    exit 1
  fi
done

# Sort files numerically by extracting leading number
sorted_files=($(printf '%s\n' "${r_files[@]}" | sort -t/ -k2 -n))

# Get the directory where this script is located
script_dir="${0:A:h}"

# Execute each R file with Rscript, respecting .Rprofile
for file in "${sorted_files[@]}"; do
  echo "Executing: $(basename "$file")"
  (cd "$script_dir" && Rscript "$file")
  if [[ $? -ne 0 ]]; then
    echo "Error: Failed to execute '$file'" >&2
    exit 1
  fi
done

echo "All R scripts executed successfully"
