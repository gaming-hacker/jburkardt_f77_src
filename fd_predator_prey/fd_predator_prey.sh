#!/bin/bash
#
gfortran -c fd_predator_prey.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd_predator_prey.f90"
  exit
fi
#
gfortran fd_predator_prey.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fd_predator_prey.o"
  exit
fi
rm fd_predator_prey.o
#
mv a.out ~/binf77/fd_predator_prey
#
echo "Executable installed as ~/binf77/fd_predator_prey"
