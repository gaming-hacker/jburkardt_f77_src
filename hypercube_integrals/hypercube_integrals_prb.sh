#!/bin/bash
#
gfortran -c hypercube_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypercube_integrals_prb.f"
  exit
fi
#
gfortran hypercube_integrals_prb.o -L$HOME/libf77 -lhypercube_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypercube_integrals_prb.o"
  exit
fi
rm hypercube_integrals_prb.o
#
mv a.out hypercube_integrals_prb
./hypercube_integrals_prb > hypercube_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hypercube_integrals_prb"
  exit
fi
rm hypercube_integrals_prb
#
echo "Test program output written to hypercube_integrals_prb_output.txt."
