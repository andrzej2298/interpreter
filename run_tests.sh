#!/bin/bash

HEADING="\033[1;32m"
OUT="\033[0;37m"
ERR="\033[1;31m"
NC="\033[0m"

color() (set -o pipefail;"$@" 2>&1>&3|sed $'s,.*,\e[1;31m&\e[m,'>&2) 3>&1

for file in $(find tests/*.c)
do
    printf "${HEADING} ---------- $file ---------- ${NC}\n"
    color ./interpreter "$file"
done

# printf "${ERR}"
# eval ./interpreter "$file" | while read line
# do
#     printf "${OUT}"  ## blue
#     echo "$line"
#     printf "${ERR}"  ## blue
# done
# printf "${NC}"
