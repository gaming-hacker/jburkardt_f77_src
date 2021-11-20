#!/bin/bash
#
gfortran -c blas3_c_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling blas3_c_prb.f"
  exit
fi
#
gfortran blas3_c_prb.o -L$HOME/libf77 -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading blas3_c_prb.o"
  exit
fi
rm blas3_c_prb.o
#
mv a.out blas3_c_prb
./blas3_c_prb > blas3_c_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running blas3_c_prb"
  exit
fi
rm blas3_c_prb
#
echo "Test program output written to blas3_c_prb_output.txt."
