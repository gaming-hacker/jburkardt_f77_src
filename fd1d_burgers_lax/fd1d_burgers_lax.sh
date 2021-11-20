#!/bin/bash
#
gfortran -c fd1d_burgers_lax.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling fd1d_burgers_lax.f"
  exit
fi
#
gfortran fd1d_burgers_lax.o
if [ $? -ne 0 ]; then
  echo "Errors while loading fd1d_burgers_lax.o"
  exit
fi
rm fd1d_burgers_lax.o
#
mv a.out ~/binf77/fd1d_burgers_lax
#
echo "Program installed as ~/binf77/fd1d_burgers_lax"
