#!/bin/sh

fswatch -o aoc.h $1.c | xargs -n1 -I{} ./build_and_run.sh $1
