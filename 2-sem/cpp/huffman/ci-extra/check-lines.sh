#!/bin/bash

exit_code=0

error() {
  exit_code=1
  echo "Error in $1: ${*:2}" >&2
}

for file in $(git grep --cached -Il ''); do
  if [[ -n "$(dos2unix <"$file" | cmp "$file")" ]]; then
    error "$file" "Found CRLF"
  fi
  if [[ -s "$file" && -n "$(tail -c 1 "$file")" ]]; then
    error "$file" "Doesn't end with a newline character"
  fi
done

exit "$exit_code"
