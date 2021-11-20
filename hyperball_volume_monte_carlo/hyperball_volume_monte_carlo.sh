#!/bin/bash
#
gfortran -c hyperball_volume_monte_carlo.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hyperball_volume_monte_carlo.f"
  exit
fi
#
gfortran hyperball_volume_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hyperball_volume_monte_carlo.o"
  exit
fi
rm hyperball_volume_monte_carlo.o
#
mv a.out ~/binf77/hyperball_volume_monte_carlo
#
echo "Executable installed as ~/binf77/hyperball_volume_monte_carlo"
