#!/bin/bash
#
gfortran -c wedge_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wedge_integrals_prb.f"
  exit
fi
#
gfortran wedge_integrals_prb.o -L$HOME/libf77 -lwedge_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wedge_integrals_prb.o"
  exit
fi
rm wedge_integrals_prb.o
#
mv a.out wedge_integrals_prb
./wedge_integrals_prb > wedge_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wedge_integrals_prb"
  exit
fi
rm wedge_integrals_prb
#
echo "Test program output written to wedge_integrals_prb_output.txt."
