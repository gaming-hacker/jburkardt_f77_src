#!/bin/bash
#
gfortran -c hypersphere_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypersphere_integrals_prb.f"
  exit
fi
#
gfortran hypersphere_integrals_prb.o -L$HOME/libf77 -lhypersphere_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypersphere_integrals_prb.o"
  exit
fi
rm hypersphere_integrals_prb.o
#
mv a.out hypersphere_integrals_prb
./hypersphere_integrals_prb > hypersphere_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hypersphere_integrals_prb"
  exit
fi
rm hypersphere_integrals_prb
#
echo "Test program output written to hypersphere_integrals_prb_output.txt."
