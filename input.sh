#! /bin/sh
SPECIFIC_DATE=$1
if [ -z $SPECIFIC_DATE ]; then
    DATE=$(date | awk '{print $2}')
    curl --cookie session-cookie https://adventofcode.com/2023/day/$DATE/input > "/home/sebastian/Documents/git/aoc23/inputs/day$DATE"
else
    curl --cookie session-cookie https://adventofcode.com/2023/day/$SPECIFIC_DATE/input > "/home/sebastian/Documents/git/aoc23/inputs/day$SPECIFIC_DATE"
fi
