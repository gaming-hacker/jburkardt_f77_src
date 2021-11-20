#!/bin/bash
#
gfortran -c pce_burgers.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pce_burgers.f"
  exit
fi
#
gfortran pce_burgers.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pce_burgers.o"
  exit
fi
rm pce_burgers.o
#
mv a.out ~/binf77/pce_burgers
#
echo "Executable installed as ~/binf77/pce_burgers"
