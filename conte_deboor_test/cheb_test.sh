#! /bin/bash
#
gfortran -c -Wall cheb_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cheb_test cheb_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cheb_test.o
#
./cheb_test > cheb_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cheb_test
#
echo "Normal end of execution."
