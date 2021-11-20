#! /bin/bash
#
gfortran -c -Wall toms379_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran toms379_test.o -L$HOME/libf77 -ltoms379
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toms379_test.o
#
mv a.out toms379_test
./toms379_test > toms379_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toms379_test
#
echo "Normal end of execution."
