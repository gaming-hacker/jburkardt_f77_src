#!/bin/bash
#
gfortran -c sort_test.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sort_test.f"
  exit
fi
#
gfortran sort_test.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sort_test.o"
  exit
fi
rm sort_test.o
#
mv a.out sort_test
./sort_test > sort_test_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sort_test"
  exit
fi
rm sort_test
#
echo "Program output written to sort_test_output.txt"
