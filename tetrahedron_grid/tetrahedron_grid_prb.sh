#!/bin/bash
#
gfortran -c tetrahedron_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tetrahedron_grid_prb.f"
  exit
fi
#
gfortran tetrahedron_grid_prb.o -L$HOME/libf77 -ltetrahedron_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tetrahedron_grid_prb.o"
  exit
fi
rm tetrahedron_grid_prb.o
#
mv a.out tetrahedron_grid_prb
./tetrahedron_grid_prb > tetrahedron_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running tetrahedron_grid_prb"
  exit
fi
rm tetrahedron_grid_prb
#
echo "Test program output written to tetrahedron_grid_prb_output.txt."
