#!/bin/bash
#
gfortran -c netode.f
if [ $? -ne 0 ]; then
  echo "Errors compiling netode.f"
  exit
fi
#
gfortran netode.o -L$HOME/libf77 -llsodi -lmachine
if [ $? -ne 0 ]; then
  echo "Errors loading args.o"
  exit
fi
rm netode.o
#
mv a.out ~/binf77/netode
#
echo "Executable installed as ~/binf77/netode."
