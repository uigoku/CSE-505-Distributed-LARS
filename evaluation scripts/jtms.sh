#!/bin/bash

for i in `seq 1 20`; do
	echo "Window size: " $i 1>&2
	distributed-sr evaluate total-time mmedia-at -w $i -s --ticker ticker/ticker-assembly-1.0.jar --tick-interval 0.1
done 2> ticker-jtms
