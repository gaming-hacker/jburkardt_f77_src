#!/bin/bash
#
gfortran -c fem2d_bvp_quadratic_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem2d_bvp_quadratic_prb.f"
  exit
fi
#
gfortran fem2d_bvp_quadratic_prb.o -L$HOME/libf77 -lfem2d_bvp_quadratic
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_bvp_quadratic_prb.o"
  exit
fi
rm fem2d_bvp_quadratic_prb.o
#
mv a.out fem2d_bvp_quadratic_prb
./fem2d_bvp_quadratic_prb > fem2d_bvp_quadratic_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fem2d_bvp_quadratic_prb"
  exit
fi
rm fem2d_bvp_quadratic_prb
#
echo "Program output written to fem2d_bvp_quadratic_prb_output.txt"
