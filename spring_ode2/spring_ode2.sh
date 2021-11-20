#!/bin/bash
#
gfortran -c spring_ode2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling spring_ode2.f"
  exit
fi
#
gfortran spring_ode2.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading spring_ode2.o"
  exit
fi
rm spring_ode2.o
#
mv a.out ~/binf77/spring_ode2
#
echo "Executable installed as ~/binf77/spring_ode2"
