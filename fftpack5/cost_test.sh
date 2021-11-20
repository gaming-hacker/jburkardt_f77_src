#!/bin/bash
#
gfortran -c cost_test.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cost_test.f"
  exit
fi
#
gfortran cost_test.o -L$HOME/libf77 -lfftpack5
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cost_test.o"
  exit
fi
rm cost_test.o
#
mv a.out cost_test
./cost_test > cost_test_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cost_test"
  exit
fi
rm cost_test
#
echo "Test results written to cost_test_output.txt."
