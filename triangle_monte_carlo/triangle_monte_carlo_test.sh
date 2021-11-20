#! /bin/bash
#
gfortran -c -g triangle_monte_carlo_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_monte_carlo_test.o -L$HOME/libf77 -ltriangle_monte_carlo
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_monte_carlo_test.o
#
mv a.out triangle_monte_carlo_test
./triangle_monte_carlo_test > triangle_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_monte_carlo_test
#
echo "Normal end of execution."
