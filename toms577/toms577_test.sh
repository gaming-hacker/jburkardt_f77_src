#! /bin/bash
#
gfortran -c -Wall toms577_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o toms577_test toms577_test.o -L$HOME/libf77 -ltoms577
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toms577_test.o
#
./toms577_test > toms577_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toms577_test
#
echo "Normal end of execution."
