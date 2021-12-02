#!/bin/bash
NUM=1000
CNT=0
while read -r line
do
    if [ "$line" -gt "$NUM" ]; then
       CNT="$(expr $CNT + 1)"
    fi
    NUM=$line
done < $1
echo $CNT
