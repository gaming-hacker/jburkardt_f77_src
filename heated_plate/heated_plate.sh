#!/bin/bash
#
gfortran -c heated_plate.f
if [ $? -ne 0 ]; then
  echo "Errors compiling heated_plate.f"
  exit
fi
#
gfortran heated_plate.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading heated_plate.o"
  exit
fi
rm heated_plate.o
#
chmod ugo+x a.out
mv a.out ~/binf77/heated_plate
#
echo "The program has been installed as ~/binf77/heated_plate."
