#!/bin/bash
#
gfortran -c fem2d_bvp_serene_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem2d_bvp_serene_prb.f"
  exit
fi
#
gfortran -o fem2d_bvp_serene_prb fem2d_bvp_serene_prb.o -L$HOME/libf77 -lfem2d_bvp_serene
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_bvp_serene_prb.o"
  exit
fi
rm fem2d_bvp_serene_prb.o
#
./fem2d_bvp_serene_prb > fem2d_bvp_serene_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fem2d_bvp_serene_prb"
  exit
fi
rm fem2d_bvp_serene_prb
#
echo "Test program output written to fem2d_bvp_serene_prb_output.txt."
