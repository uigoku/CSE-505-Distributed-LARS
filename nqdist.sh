#!/bin/bash

for i in `seq 2 2 6`; do
	echo "Stages: " $i 1>&2
	distributed-sr evaluate load-test nqueens-$i -u 1.0 -l 0.1
done 2> load-distributed
