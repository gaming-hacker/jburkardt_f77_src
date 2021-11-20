#! /bin/bash
#
gfortran -c -Wall muller_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o muller_test muller_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm muller_test.o
#
./muller_test > muller_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm muller_test
#
echo "Normal end of execution."
