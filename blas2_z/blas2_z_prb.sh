#!/bin/bash
#
gfortran -c blas2_z_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling blas2_z_prb.f"
  exit
fi
#
gfortran blas2_z_prb.o -L$HOME/libf77 -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading blas2_z_prb.o"
  exit
fi
rm blas2_z_prb.o
#
mv a.out blas2_z_prb
./blas2_z_prb > blas2_z_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running blas2_z_prb"
  exit
fi
rm blas2_z_prb
#
echo "Test program output written to blas2_z_prb_output.txt."
