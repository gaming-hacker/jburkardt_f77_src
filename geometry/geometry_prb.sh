#!/bin/bash
#
gfortran -c geometry_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling geometry_prb.f"
  exit
fi
#
gfortran geometry_prb.o -L$HOME/libf77 -lgeometry
if [ $? -ne 0 ]; then
  echo "Errors linking and loading geometry_prb.o"
  exit
fi
rm geometry_prb.o
#
mv a.out geometry_prb
./geometry_prb > geometry_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running geometry_prb"
  exit
fi
rm geometry_prb
#
echo "Test results written to geometry_prb_output.txt."
