#!/bin/bash
#
gfortran -c line_cvt_lloyd_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling line_cvt_lloyd_prb.f"
  exit
fi
#
gfortran -o line_cvt_lloyd_prb line_cvt_lloyd_prb.o -L$HOME/libf77 -lline_cvt_lloyd
if [ $? -ne 0 ]; then
  echo "Errors linking and loading line_cvt_lloyd_prb.o"
  exit
fi
rm line_cvt_lloyd_prb.o
#
./line_cvt_lloyd_prb > line_cvt_lloyd_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running line_cvt_lloyd_prb"
  exit
fi
rm line_cvt_lloyd_prb
#
echo "Test program output written to line_cvt_lloyd_prb_output.txt."
