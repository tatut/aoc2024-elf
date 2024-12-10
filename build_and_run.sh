#!/bin/sh
echo "----------------------"
echo "Run and build: $1"

clang -o where_exe $1.c
./where_exe
