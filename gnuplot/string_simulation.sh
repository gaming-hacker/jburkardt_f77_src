#!/bin/bash
#
gfortran -c string_simulation.f
if [ $? -ne 0 ]; then
  echo "Errors compiling string_simulation.f"
  exit
fi
#
gfortran string_simulation.o
if [ $? -ne 0 ]; then
  echo "Errors linking string_simulation.o"
  exit
fi
#
rm string_simulation.o
mv a.out ~/binf77/string_simulation
#
echo "Executable installed as ~/binf77/string_simulation"
