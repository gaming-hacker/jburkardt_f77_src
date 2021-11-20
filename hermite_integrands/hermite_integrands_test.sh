#! /bin/bash
#
gfortran -c -Wall hermite_integrands_test.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hermite_integrands_test.f"
  exit
fi
#
gfortran hermite_integrands_test.o -L$HOME/libf77 -lhermite_integrands
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hermite_integrands_test.o"
  exit
fi
rm hermite_integrands_test.o
#
mv a.out hermite_integrands_test
./hermite_integrands_test > hermite_integrands_test_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hermite_integrands_test"
  exit
fi
rm hermite_integrands_test
#
echo "Test program output written to hermite_integrands_test_output.txt."
