#!/bin/bash
#
gfortran -c tcell_mass.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tcell_mass.f"
  exit
fi
#
gfortran tcell_mass.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tcell_mass.o"
  exit
fi
rm tcell_mass.o
#
chmod ugo+x a.out
mv a.out ~/binf77/tcell_mass
#
echo "Executable installed as ~/binf77/tcell_mass"
