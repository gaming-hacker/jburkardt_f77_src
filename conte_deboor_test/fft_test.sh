#! /bin/bash
#
gfortran -c -Wall fft_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fft_test fft_test.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fft_test.o
#
./fft_test > fft_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fft_test
#
echo "Normal end of execution."
