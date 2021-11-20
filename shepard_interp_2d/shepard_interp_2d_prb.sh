#!/bin/bash
#
gfortran -c shepard_interp_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling shepard_interp_2d_prb.f"
  exit
fi
#
gfortran shepard_interp_2d_prb.o -L$HOME/libf77 -lshepard_interp_2d -ltest_interp_2d -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading shepard_interp_2d_prb.o"
  exit
fi
rm shepard_interp_2d_prb.o
#
mv a.out shepard_interp_2d_prb
./shepard_interp_2d_prb > shepard_interp_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running shepard_interp_2d_prb"
  exit
fi
rm shepard_interp_2d_prb
#
echo "Test program output written to shepard_interp_2d_prb_output.txt."
