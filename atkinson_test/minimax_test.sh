#! /bin/bash
#
gfortran -c -Wall minimax_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o minimax_test minimax_test.o -L$HOME/libf77 -latkinson
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm minimax_test.o
#
./minimax_test > minimax_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm minimax_test
#
echo "Normal end of execution."
