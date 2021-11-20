#!/bin/bash
#
gfortran -c spline_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling spline_prb.f"
  exit
fi
#
gfortran spline_prb.o -L$HOME/libf77 -lspline
if [ $? -ne 0 ]; then
  echo "Errors linking and loading spline_prb.o"
  exit
fi
rm spline_prb.o
#
mv a.out spline_prb
./spline_prb > spline_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running spline_prb"
  exit
fi
rm spline_prb
#
echo "Test program output written to spline_prb_output.txt."
