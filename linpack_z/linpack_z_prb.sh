#!/bin/bash
#
gfortran -c linpack_z_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling linpack_z_prb.f"
  exit
fi
#
gfortran linpack_z_prb.o -L$HOME/libf77 -llinpack_z -lblas1_z -lblas1_d
if [ $? -ne 0 ]; then
  echo "Errors linking and loading linpack_z_prb.o"
  exit
fi
rm linpack_z_prb.o
#
mv a.out linpack_z_prb
./linpack_z_prb > linpack_z_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running linpack_z_prb"
  exit
fi
rm linpack_z_prb
#
echo "Test results written to linpack_z_prb_output.txt."
