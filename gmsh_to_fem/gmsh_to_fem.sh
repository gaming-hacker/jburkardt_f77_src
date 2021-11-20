#!/bin/bash
#
gfortran -c gmsh_to_fem.f
if [ $? -ne 0 ]; then
  echo "Errors compiling gmsh_to_fem.f"
  exit
fi
#
gfortran gmsh_to_fem.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading gmsh_to_fem.o"
  exit
fi
#
rm gmsh_to_fem.o
#
chmod ugo+x a.out
mv a.out ~/binf77/gmsh_to_fem
#
echo "Executable installed as ~/binf77/gmsh_to_fem"
