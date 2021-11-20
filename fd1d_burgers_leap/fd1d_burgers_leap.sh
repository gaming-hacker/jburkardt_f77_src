#!/bin/bash
#
gfortran -c fd1d_burgers_leap.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling fd1d_burgers_leap.f"
  exit
fi
#
gfortran fd1d_burgers_leap.o
if [ $? -ne 0 ]; then
  echo "Errors while loading fd1d_burgers_leap.o"
  exit
fi
rm fd1d_burgers_leap.o
#
mv a.out ~/binf77/fd1d_burgers_leap
#
echo "Program installed as ~/binf77/fd1d_burgers_leap"
