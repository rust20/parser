#!/bin/bash
ulimit -s unlimited
make release
./main -f test_large.b
./main -f test_large_10.b
./main -f test_large_100.b
./main -f test_large_1000.b

