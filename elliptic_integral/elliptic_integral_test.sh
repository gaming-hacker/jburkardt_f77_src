#! /bin/bash
#
gfortran -c -Wall elliptic_integral_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o elliptic_integral_test elliptic_integral_test.o -L$HOME/libf77 -lelliptic_integral
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm elliptic_integral_test.o
#
./elliptic_integral_test > elliptic_integral_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm elliptic_integral_test
#
echo "Normal end of execution."
