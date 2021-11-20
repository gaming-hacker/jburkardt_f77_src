#! /bin/bash
#
gfortran -c -Wall r8lib_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8lib_test r8lib_test.o $HOME/libf77/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8lib_test.o
#
./r8lib_test > r8lib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8lib_test
#
echo "Normal end of execution."
