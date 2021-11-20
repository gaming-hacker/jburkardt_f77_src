#!/bin/bash
#
gfortran -c fd1d_advection_ftcs.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd1d_advection_ftcs.f"
  exit
fi
#
gfortran fd1d_advection_ftcs.o
if [ $? -ne 0 ]; then
  echo "Errors linking fd1d_advection_ftcs.o"
  exit
fi
#
rm fd1d_advection_ftcs.o
mv a.out ~/binf77/fd1d_advection_ftcs
#
echo "Executable installed as ~/binf77/fd1d_advection_ftcs"
