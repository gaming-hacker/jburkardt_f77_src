#!/bin/bash
#
gfortran -c driver.f
if [ $? -ne 0 ]; then
  echo "Errors compiling driver.f"
  exit
fi
#
gfortran driver.o -L$HOME/libf77 -ltoms425
if [ $? -ne 0 ]; then
  echo "Errors linking and loading driver.o"
  exit
fi
rm driver.o
#
mv a.out driver
./driver > res
if [ $? -ne 0 ]; then
  echo "Errors running driver"
  exit
fi
rm driver
#
echo "Test results written to res."
