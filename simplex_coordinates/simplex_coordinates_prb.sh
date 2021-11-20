#!/bin/bash
#
gfortran -c simplex_coordinates_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling simplex_coordinates_prb.f"
  exit
fi
#
gfortran simplex_coordinates_prb.o -L$HOME/libf77 -lsimplex_coordinates
if [ $? -ne 0 ]; then
  echo "Errors linking and loading simplex_coordinates_prb.o"
  exit
fi
rm simplex_coordinates_prb.o
#
mv a.out simplex_coordinates_prb
./simplex_coordinates_prb > simplex_coordinates_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running simplex_coordinates_prb"
  exit
fi
rm simplex_coordinates_prb
#
echo "Test program output written to simplex_coordinates_prb_output.txt."
