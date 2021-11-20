#!/bin/bash
#
gfortran -c sint_test.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sint_test.f"
  exit
fi
#
gfortran sint_test.o -L$HOME/libf77 -lfftpack5
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sint_test.o"
  exit
fi
rm sint_test.o
#
mv a.out sint_test
./sint_test > sint_test_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sint_test"
  exit
fi
rm sint_test
#
echo "Test results written to sint_test_output.txt."
