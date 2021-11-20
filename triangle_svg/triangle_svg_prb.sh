#!/bin/bash
#
gfortran -c triangle_svg_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle_svg_prb.f"
  exit
fi
#
gfortran triangle_svg_prb.o -L$HOME/libf77 -ltriangle_svg
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle_svg_prb.o"
  exit
fi
rm triangle_svg_prb.o
#
mv a.out triangle_svg_prb
./triangle_svg_prb > triangle_svg_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running triangle_svg_prb"
  exit
fi
rm triangle_svg_prb
#
echo "Test program output written to triangle_svg_prb_output.txt."
