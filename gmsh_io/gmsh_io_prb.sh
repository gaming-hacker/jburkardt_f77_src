#!/bin/bash
#
gfortran -c gmsh_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling gmsh_io_prb.f"
  exit
fi
#
gfortran -o gmsh_io_prb gmsh_io_prb.o -L$HOME/libf77 -lgmsh_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading gmsh_io_prb.o"
  exit
fi
rm gmsh_io_prb.o
#
./gmsh_io_prb > gmsh_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running gmsh_io_prb"
  exit
fi
rm gmsh_io_prb
#
echo "Test results written to gmsh_io_prb_output.txt."
