#! /bin/bash
#
gfortran -c -Wall noise_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o noise_test noise_test.o -L$HOME/libf77 -latkinson
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm noise_test.o
#
./noise_test > noise_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm noise_test
#
echo "Normal end of execution."
