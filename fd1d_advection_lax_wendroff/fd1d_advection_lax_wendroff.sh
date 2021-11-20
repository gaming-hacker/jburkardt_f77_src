#!/bin/bash
#
gfortran -c fd1d_advection_lax_wendroff.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd1d_advection_lax_wendroff.f"
  exit
fi
#
gfortran fd1d_advection_lax_wendroff.o
if [ $? -ne 0 ]; then
  echo "Errors linking fd1d_advection_lax_wendroff.o"
  exit
fi
#
rm fd1d_advection_lax_wendroff.o
mv a.out ~/binf77/fd1d_advection_lax_wendroff
#
echo "Executable installed as ~/binf77/fd1d_advection_lax_wendroff"
