#!/bin/bash
#
gfortran -c jacobi_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling jacobi_prb.f"
  exit
fi
#
gfortran jacobi_prb.o -L$HOME/libf77 -ljacobi
if [ $? -ne 0 ]; then
  echo "Errors linking and loading jacobi_prb.o"
  exit
fi
rm jacobi_prb.o
#
mv a.out jacobi_prb
./jacobi_prb > jacobi_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running jacobi_prb"
  exit
fi
rm jacobi_prb
#
echo "Test results written to jacobi_prb_output.txt."
