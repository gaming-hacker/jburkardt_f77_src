#!/bin/bash
#
gfortran -c disk_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling disk_grid_prb.f"
  exit
fi
#
gfortran disk_grid_prb.o -L$HOME/libf77 -ldisk_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading disk_grid_prb.o"
  exit
fi
rm disk_grid_prb.o
#
mv a.out disk_grid_prb
./disk_grid_prb > disk_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running disk_grid_prb"
  exit
fi
rm disk_grid_prb
#
echo "Test program output written to disk_grid_prb_output.txt."
