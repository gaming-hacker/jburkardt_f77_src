#! /bin/bash
#
gfortran -c disk01_monte_carlo_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran disk01_monte_carlo_test.o -L$HOME/libf77 -ldisk01_monte_carlo
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk01_monte_carlo_test.o
#
mv a.out disk01_monte_carlo_test
./disk01_monte_carlo_test > disk01_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk01_monte_carlo_test
#
echo "Normal end of execution."
