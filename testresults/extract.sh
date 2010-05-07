#!/bin/sh

cat $1 | grep "Optimise: $2" | awk '{print $4}' | cut -d ')' -f 1 > indexes
cat $1 | grep "Optimise: $2" | cut -d ')' -f 2 |  awk -F "," '{print $2}' > values

paste indexes values

#rm indexes
#rm values

