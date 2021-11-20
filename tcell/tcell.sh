#!/bin/bash
#
gfortran -c tcell.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tcell.f"
  exit
fi
#
gfortran tcell.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tcell.o"
  exit
fi
rm tcell.o
#
chmod ugo+x a.out
mv a.out ~/binf77/tcell
#
echo "Program installed as ~/binf77/tcell"
