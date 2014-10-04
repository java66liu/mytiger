#!/bin/bash

if [ -n "$1" ]; then 
	prog=$1
else
	prog=a.out
fi

for i in {1..49};
do
	echo $i
	./$prog	../testcases/test$i.tig
	echo "----------------------"
done

echo "merge"
./$prog	../testcases/merge.tig
echo "----------------------"

echo "queen"
./$prog	../testcases/queens.tig
echo "----------------------"
