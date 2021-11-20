#!/bin/bash
#
gfortran -c sphere_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sphere_integrals_prb.f"
  exit
fi
#
gfortran sphere_integrals_prb.o -L$HOME/libf77 -lsphere_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sphere_integrals_prb.o"
  exit
fi
rm sphere_integrals_prb.o
#
mv a.out sphere_integrals_prb
./sphere_integrals_prb > sphere_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sphere_integrals_prb"
  exit
fi
rm sphere_integrals_prb
#
echo "Test program output written to sphere_integrals_prb_output.txt."
