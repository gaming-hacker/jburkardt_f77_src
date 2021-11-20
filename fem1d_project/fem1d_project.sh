#!/bin/bash
#
gfortran -c fem1d_project.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem1d_project.f"
  exit
fi
#
gfortran fem1d_project.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_project.o"
  exit
fi
#
rm fem1d_project.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem1d_project
#
echo "Program installed as ~/binf77/fem1d_project"
