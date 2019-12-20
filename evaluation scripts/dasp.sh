#!/bin/bash

for i in `seq 1 20`; do
	echo "Windoww size: " $i 1>&2
	distributed-sr evaluate total-time mmedia -w $i
done 2> distributed-asp
