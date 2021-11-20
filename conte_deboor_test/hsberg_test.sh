#! /bin/bash
#
gfortran -c -Wall hsberg_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hsberg_test hsberg_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hsberg_test.o
#
./hsberg_test > hsberg_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hsberg_test
#
echo "Normal end of execution."
