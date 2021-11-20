#!/bin/bash
#
gfortran -c lorenz_ode.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lorenz_ode.f"
  exit
fi
#
gfortran lorenz_ode.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lorenz_ode.o"
  exit
fi
rm lorenz_ode.o
#
mv a.out ~/binf77/lorenz_ode
#
echo "Executable installed as ~/binf77/lorenz_ode"
