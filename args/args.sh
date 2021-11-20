#!/bin/bash
#
gfortran -c args.f
if [ $? -ne 0 ]; then
  echo "Errors compiling args.f"
  exit
fi
#
gfortran args.o
if [ $? -ne 0 ]; then
  echo "Errors loading args.o"
  exit
fi
rm args.o
#
chmod ugo+x args
mv a.out ~/binf77/args
#
echo "Executable installed as ~/binf77/args."
