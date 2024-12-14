#!/bin/sh
echo "----------------------"
echo "Run and build: $1"

# use fast for best speed (but slower compilation)
OPT=${2:-0}

FLAGS=$(pkg-config --cflags --libs raylib)

rm where_exe
clang $FLAGS -O$OPT -o where_exe $1.c && ./where_exe
