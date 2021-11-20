#!/bin/bash
#
gfortran -c tetrahedron_properties.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tetrahedron_properties.f"
  exit
fi
#
gfortran tetrahedron_properties.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tetrahedron_properties.o"
  exit
fi
rm tetrahedron_properties.o
#
chmod ugo+x a.out
mv a.out ~/binf77/tetrahedron_properties
#
echo "Executable installed as ~/binf77/tetrahedron_properties"
