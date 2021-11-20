#!/bin/bash
#
gfortran -c toms683_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran toms683_test.o -L$HOME/libf77 -ltoms683
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toms683_test.o
#
mv a.out toms683_test
./toms683_test > toms683_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toms683_test
#
echo "Normal end of execution."
