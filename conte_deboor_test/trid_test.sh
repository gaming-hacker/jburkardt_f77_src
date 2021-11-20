#! /bin/bash
#
gfortran -c -Wall trid_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o trid_test trid_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm trid_test.o
#
./trid_test > trid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm trid_test
#
echo "Normal end of execution."
