#!/bin/bash
#
gfortran -c fd1d_advection_diffusion_steady.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd1d_advection_diffusion_steady.f"
  exit
fi
#
gfortran fd1d_advection_diffusion_steady.o
if [ $? -ne 0 ]; then
  echo "Errors linking fd1d_advection_diffusion_steady.o"
  exit
fi
#
rm fd1d_advection_diffusion_steady.o
mv a.out ~/binf77/fd1d_advection_diffusion_steady
#
echo "Executable installed as ~/binf77/fd1d_advection_diffusion_steady"
