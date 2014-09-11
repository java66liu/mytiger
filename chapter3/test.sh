#/bin/sh



for i in {1..49};
do
	echo $i
	./$1	../testcases/test$i.tig
	echo "----------------------"
done

echo "merge"
./$1 ../testcases/merge.tig
echo "----------------------"

echo "queen"
./$1 ../testcases/queens.tig
echo "----------------------"

