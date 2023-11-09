#!/bin/bash
ulimit -s unlimited
make find_all

echo "# file with huge function"
valgrind --tool=massif --massif-out-file=./massif.out.large ./main -f test_large.b

echo "# file containing 10 huge function"
valgrind --tool=massif --massif-out-file=./massif.out.large10 ./main -f test_large_10.b

echo "# file containing 100 huge function"
valgrind --tool=massif --massif-out-file=./massif.out.large100 ./main -f test_large_100.b

echo "# file containing 1000 huge function"massif.out.
valgrind --tool=massif --massif-out-file=./massif.out.large1000 ./main -f test_large_1000.b
  
