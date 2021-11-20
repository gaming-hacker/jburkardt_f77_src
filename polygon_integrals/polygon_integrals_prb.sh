#!/bin/bash
#
gfortran -c polygon_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling polygon_integrals_prb.f"
  exit
fi
#
gfortran polygon_integrals_prb.o -L$HOME/libf77 -lpolygon_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading polygon_integrals_prb.o"
  exit
fi
rm polygon_integrals_prb.o
#
mv a.out polygon_integrals_prb
./polygon_integrals_prb > polygon_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running polygon_integrals_prb"
  exit
fi
rm polygon_integrals_prb
#
echo "Test program output written to polygon_integrals_prb_output.txt."
