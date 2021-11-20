#! /bin/bash
#
gfortran -c -Wall mrgfls_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o mrgfls_test mrgfls_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mrgfls_test.o
#
./mrgfls_test > mrgfls_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mrgfls_test
#
echo "Normal end of execution."
