#!/bin/bash

# get arguments

infile=$1
inregion=$2
window=$3

# settings

info=($(echo $inregion | tr ":-" " "))

chromosome=${info[0]}
n=${#info[@]}

if [[ "$n" == 1 ]]
then

	start=0
	end=100000000000

else

	start=${info[1]}
	end=${info[2]}

fi

# make contact matrix

tabix $infile $inregion | \
	awk -v w=$window -v OFS="\t" -v s=$start -v e=$end \
	'$1 == $4 && $5 >= s && $5 <= e{i = int($2 / w) * w; j = int($5 / w) * w; a[$1 OFS i OFS j] += 1}END{for(k in a) print k, a[k]}'
