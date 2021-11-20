#!/bin/bash
#
gfortran -c cube_exactness_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cube_exactness_prb.f"
  exit
fi
#
gfortran cube_exactness_prb.o -L$HOME/libf77 -lcube_exactness
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cube_exactness_prb.o"
  exit
fi
rm cube_exactness_prb.o
#
mv a.out cube_exactness_prb
./cube_exactness_prb > cube_exactness_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cube_exactness_prb"
  exit
fi
rm cube_exactness_prb
#
echo "Test program output written to cube_exactness_prb_output.txt."
