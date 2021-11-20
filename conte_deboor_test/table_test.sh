#! /bin/bash
#
gfortran -c -Wall table_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o table_test table_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_test.o
#
./table_test > table_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm table_test
#
echo "Normal end of execution."
