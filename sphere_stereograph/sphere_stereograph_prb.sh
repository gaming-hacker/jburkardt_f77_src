#!/bin/bash
#
gfortran -c sphere_stereograph_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sphere_stereograph_prb.f"
  exit
fi
#
gfortran sphere_stereograph_prb.o -L$HOME/libf77 -lsphere_stereograph
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sphere_stereograph_prb.o"
  exit
fi
rm sphere_stereograph_prb.o
#
mv a.out sphere_stereograph_prb
./sphere_stereograph_prb > sphere_stereograph_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sphere_stereograph_prb"
  exit
fi
rm sphere_stereograph_prb
#
echo "Test program output written to sphere_stereograph_prb_output.txt."
