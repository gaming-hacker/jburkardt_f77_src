#! /bin/bash
#
gfortran -c -Wall toms449_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o toms449_test toms449_test.o -L$HOME/libf77 -ltoms449
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toms449_test.o
#
./toms449_test > toms449_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toms449_test
#
echo "Normal end of execution."
