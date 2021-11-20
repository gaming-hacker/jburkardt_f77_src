#!/bin/bash
#
gfortran -c pyramid_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_grid_prb.f"
  exit
fi
#
gfortran pyramid_grid_prb.o -L$HOME/libf77 -lpyramid_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pyramid_grid_prb.o"
  exit
fi
rm pyramid_grid_prb.o
#
mv a.out pyramid_grid_prb
./pyramid_grid_prb > pyramid_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pyramid_grid_prb"
  exit
fi
rm pyramid_grid_prb
#
echo "Test program output written to pyramid_grid_prb_output.txt."
