#!/usr/bin/env bash
#
nix-shell -p lynx curl --run '
# Prompt the user to enter a URL
read -p "Enter the URL: " url

# Fetch the content using curl and pass it to sed for processing
content=$(curl -s "$url" | lynx -stdin -dump -width 180)

# Extract the desired text using sed and regular expressions
extracted_text=$(echo "$content" | sed -n "s/.*\[6\]\(.*\)\[7\].*/\1/p")

# Print the extracted text
if [[ -n "$extracted_text" ]]; then
  echo "$extracted_text"
  echo "* TODO Watch \"$extracted_text\" on YouTube" >> ~/org/refile.org
  echo "$url" >> ~/org/refile.org
else
  echo "Text not found."
fi
'
