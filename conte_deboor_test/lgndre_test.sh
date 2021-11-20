#! /bin/bash
#
gfortran -c -Wall lgndre_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o lgndre_test lgndre_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lgndre_test.o
#
./lgndre_test > lgndre_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lgndre_test
#
echo "Normal end of execution."
