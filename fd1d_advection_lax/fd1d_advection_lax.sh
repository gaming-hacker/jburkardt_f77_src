#!/bin/bash
#
gfortran -c fd1d_advection_lax.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd1d_advection_lax.f"
  exit
fi
#
gfortran fd1d_advection_lax.o
if [ $? -ne 0 ]; then
  echo "Errors linking fd1d_advection_lax.o"
  exit
fi
#
rm fd1d_advection_lax.o
mv a.out ~/binf77/fd1d_advection_lax
#
echo "Executable installed as ~/binf77/fd1d_advection_lax"
