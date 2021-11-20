#!/bin/bash
#
gfortran -c fair_dice_simulation.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fair_dice_simulation.f"
  exit
fi
#
gfortran fair_dice_simulation.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fair_dice_simulation.o"
  exit
fi
rm fair_dice_simulation.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fair_dice_simulation
#
echo "Executable installed as ~/binf77/fair_dice_simulation"
