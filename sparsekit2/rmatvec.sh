#!/bin/bash
#
gfortran -c rmatvec.f
if [ $? -ne 0 ]; then
  echo "Errors compiling rmatvec.f"
  exit
fi
#
gfortran rmatvec.o -L$HOME/libf77 -lsparsekit2
if [ $? -ne 0 ]; then
  echo "Errors linking and loading rmatvec.o"
  exit
fi
rm rmatvec.o
#
mv a.out rmatvec
./rmatvec > rmatvec_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running rmatvec"
  exit
fi
rm rmatvec
#
echo "Test results written to rmatvec_output.txt."
