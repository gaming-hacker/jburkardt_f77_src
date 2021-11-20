#! /bin/bash
#
gfortran -c owen_test.f
if [ $? -ne 0 ]; then
  echo "Errors compiling owen_test.f"
  exit
fi
#
gfortran owen_test.o -L$HOME/libf77 -lowen
if [ $? -ne 0 ]; then
  echo "Errors linking and loading owen_test.o"
  exit
fi
rm owen_test.o
#
mv a.out owen_test
./owen_test > owen_test.txt
if [ $? -ne 0 ]; then
  echo "Errors running owen_test"
  exit
fi
rm owen_test
#
echo "Test results written to owen_test_output.txt."
