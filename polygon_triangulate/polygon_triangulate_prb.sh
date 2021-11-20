#!/bin/bash
#
gfortran -c polygon_triangulate_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling polygon_triangulate_prb.f"
  exit
fi
#
gfortran polygon_triangulate_prb.o -L$HOME/libf77 -lpolygon_triangulate
if [ $? -ne 0 ]; then
  echo "Errors linking and loading polygon_triangulate_prb.o"
  exit
fi
rm polygon_triangulate_prb.o
#
mv a.out polygon_triangulate_prb
./polygon_triangulate_prb > polygon_triangulate_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running polygon_triangulate_prb"
  exit
fi
rm polygon_triangulate_prb
#
echo "Test program output written to polygon_triangulate_prb_output.txt."
