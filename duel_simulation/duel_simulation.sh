#!/bin/bash
#
gfortran -c duel_simulation.f
if [ $? -ne 0 ]; then
  echo "Errors compiling duel_simulation.f"
  exit
fi
#
gfortran duel_simulation.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading duel_simulation.o"
  exit
fi
rm duel_simulation.o
#
chmod ugo+x a.out
mv a.out ~/binf77/duel_simulation
#
echo "Executable installed as ~/binf77/duel_simulation"
