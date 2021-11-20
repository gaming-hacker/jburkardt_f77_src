#!/bin/bash
#
gfortran -c cube_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cube_integrals_prb.f"
  exit
fi
#
gfortran cube_integrals_prb.o -L$HOME/libf77 -lcube_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cube_integrals_prb.o"
  exit
fi
rm cube_integrals_prb.o
#
mv a.out cube_integrals_prb
./cube_integrals_prb > cube_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cube_integrals_prb"
  exit
fi
rm cube_integrals_prb
#
echo "Test program output written to cube_integrals_prb_output.txt."
