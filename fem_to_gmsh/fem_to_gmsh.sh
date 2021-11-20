#!/bin/bash
#
gfortran -c fem_to_gmsh.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem_to_gmsh.f"
  exit
fi
#
gfortran fem_to_gmsh.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem_to_gmsh.o"
  exit
fi
#
rm fem_to_gmsh.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem_to_gmsh
#
echo "Executable installed as ~/binf77/fem_to_gmsh"
