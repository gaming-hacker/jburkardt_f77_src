#!/bin/bash
#
gfortran -c -Wall brent_original_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran brent_original_test.o -L$HOME/libf77 -lbrent_original
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm brent_original_test.o
#
mv a.out brent_original_test
./brent_original_test > brent_original_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm brent_original_test
#
echo "Normal end of execution."
