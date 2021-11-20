#! /bin/bash
#
gfortran -c -Wall pcubic_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o pcubic_test pcubic_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pcubic_test.o
#
./pcubic_test > pcubic_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pcubic_test
#
echo "Normal end of execution."
