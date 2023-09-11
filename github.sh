#!/usr/bin/bash 

curl -H 'Accept: application/vnd.github.preview.text-match+json' "https://api.github.com/search/repositories?q=language:agda&per_page=1000&order=asc" | jq '.items | .[].full_name'
