#!/bin/bash
#
gfortran -c line_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling line_integrals_prb.f"
  exit
fi
#
gfortran line_integrals_prb.o -L$HOME/libf77 -lline_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading line_integrals_prb.o"
  exit
fi
rm line_integrals_prb.o
#
mv a.out line_integrals_prb
./line_integrals_prb > line_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running line_integrals_prb"
  exit
fi
rm line_integrals_prb
#
echo "Test results written to line_integrals_prb_output.txt."
