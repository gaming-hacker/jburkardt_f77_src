#!/bin/bash
#
gfortran -c -Wall test_values_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_values_test.o -L$HOME/libf77 -ltest_values
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_values_test.o
#
mv a.out test_values_test
./test_values_test > test_values_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_values_test
#
echo "Normal end of execution."
