#!/bin/bash
#
gfortran -c f77_intrinsics.f
if [ $? -ne 0 ]; then
  echo "Errors compiling f77_intrinsics.f"
  exit
fi
#
gfortran f77_intrinsics.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading f77_intrinsics.o"
  exit
fi
rm f77_intrinsics.o
#
mv a.out f77_intrinsics
./f77_intrinsics > f77_intrinsics_output.txt
rm f77_intrinsics
#
echo "Program output written to f77_intrinsics_output.txt"
