#!/bin/bash
#
gfortran -c pce_ode_hermite_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pce_ode_hermite_prb.f"
  exit
fi
#
gfortran pce_ode_hermite_prb.o -L$HOME/libf77 -lpce_ode_hermite
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pce_ode_hermite_prb.o"
  exit
fi
rm pce_ode_hermite_prb.o
#
mv a.out pce_ode_hermite_prb
./pce_ode_hermite_prb > pce_ode_hermite_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pce_ode_hermite_prb"
  exit
fi
rm pce_ode_hermite_prb
#
echo "Test program output written to pce_ode_hermite_prb_output.txt."
