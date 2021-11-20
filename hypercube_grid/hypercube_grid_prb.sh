#!/bin/bash
#
gfortran -c hypercube_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypercube_grid_prb.f"
  exit
fi
#
gfortran -o hypercube_grid_prb hypercube_grid_prb.o -L$HOME/libf77 -lhypercube_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypercube_grid_prb.o"
  exit
fi
rm hypercube_grid_prb.o
#
./hypercube_grid_prb > hypercube_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hypercube_grid_prb"
  exit
fi
rm hypercube_grid_prb
#
echo "Test program output written to hypercube_grid_prb_output.txt."
