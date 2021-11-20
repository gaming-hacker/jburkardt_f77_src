#!/bin/bash
#
gfortran -c pwl_interp_1d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pwl_interp_1d_prb.f"
  exit
fi
#
gfortran pwl_interp_1d_prb.o -L$HOME/libf77 -lpwl_interp_1d -ltest_interp -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pwl_interp_1d_prb.o"
  exit
fi
rm pwl_interp_1d_prb.o
#
mv a.out pwl_interp_1d_prb
./pwl_interp_1d_prb > pwl_interp_1d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pwl_interp_1d_prb"
  exit
fi
rm pwl_interp_1d_prb
#
echo "Test program output written to pwl_interp_1d_prb_output.txt."
