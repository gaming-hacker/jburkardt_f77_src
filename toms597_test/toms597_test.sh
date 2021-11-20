#! /bin/bash
#
gfortran -c -Wall toms597_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran toms597_test.o $HOME/libf77/toms597.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toms597_test.o
#
mv a.out toms597_test
./toms597_test > toms597_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toms597_test
#
echo "Normal end of execution."
