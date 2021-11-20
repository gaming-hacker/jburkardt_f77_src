#!/bin/bash
#
gfortran -c wedge_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wedge_grid_prb.f"
  exit
fi
#
gfortran wedge_grid_prb.o -L$HOME/libf77 -lwedge_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wedge_grid_prb.o"
  exit
fi
rm wedge_grid_prb.o
#
mv a.out wedge_grid_prb
./wedge_grid_prb > wedge_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wedge_grid_prb"
  exit
fi
rm wedge_grid_prb
#
echo "Test program output written to wedge_grid_prb_output.txt."
