#! /bin/bash
#
gfortran -c -Wall jtimesh_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o jtimesh_test jtimesh_test.o -L$HOME/libf77 -latkinson
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jtimesh_test.o
#
./jtimesh_test > jtimesh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm jtimesh_test
#
echo "Normal end of execution."
