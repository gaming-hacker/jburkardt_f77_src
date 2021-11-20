#!/bin/bash
#
gfortran -c blas2_s_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling blas2_s_prb.f"
  exit
fi
#
gfortran blas2_s_prb.o -L$HOME/libf77 -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading blas2_s_prb.o"
  exit
fi
rm blas2_s_prb.o
#
mv a.out blas2_s_prb
./blas2_s_prb > blas2_s_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running blas2_s_prb"
  exit
fi
rm blas2_s_prb
#
echo "Test program output written to blas2_s_prb_output.txt."
