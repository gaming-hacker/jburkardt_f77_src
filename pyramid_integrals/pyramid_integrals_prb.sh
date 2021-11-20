#!/bin/bash
#
gfortran -c pyramid_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_integrals_prb.f"
  exit
fi
#
gfortran pyramid_integrals_prb.o -L$HOME/libf77 -lpyramid_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pyramid_integrals_prb.o"
  exit
fi
rm pyramid_integrals_prb.o
#
mv a.out pyramid_integrals_prb
./pyramid_integrals_prb > pyramid_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pyramid_integrals_prb"
  exit
fi
rm pyramid_integrals_prb
#
echo "Test program output written to pyramid_integrals_prb_output.txt."
