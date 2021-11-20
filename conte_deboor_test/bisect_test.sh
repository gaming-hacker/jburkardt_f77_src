#! /bin/bash
#
gfortran -c -Wall bisect_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bisect_test bisect_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bisect_test.o
#
./bisect_test > bisect_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bisect_test
#
echo "Normal end of execution."
