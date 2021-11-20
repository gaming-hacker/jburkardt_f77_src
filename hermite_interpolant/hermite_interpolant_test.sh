#! /bin/bash
#
gfortran -c -Wall hermite_interpolant_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hermite_interpolant_test.o -L$HOME/libf77 -lhermite_interpolant
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_interpolant_test.o
#
mv a.out hermite_interpolant_test
./hermite_interpolant_test > hermite_interpolant_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hermite_interpolant_test
#
echo "Normal end of execution."
