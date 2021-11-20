#!/bin/bash
#
gfortran -c polygon_properties_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling polygon_properties_prb.f"
  exit
fi
#
gfortran polygon_properties_prb.o -L$HOME/libf77 -lpolygon_properties -lpolygon_triangulate
if [ $? -ne 0 ]; then
  echo "Errors linking and loading polygon_properties_prb.o"
  exit
fi
rm polygon_properties_prb.o
#
mv a.out polygon_properties_prb
./polygon_properties_prb > polygon_properties_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running polygon_properties_prb"
  exit
fi
rm polygon_properties_prb
#
echo "Test program output written to polygon_properties_prb_output.txt."
