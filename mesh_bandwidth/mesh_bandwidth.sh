#!/bin/bash
#
gfortran -c mesh_bandwidth.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mesh_bandwidth.f"
  exit
fi
#
gfortran mesh_bandwidth.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mesh_bandwidth.o"
  exit
fi
rm mesh_bandwidth.o
#
chmod ugo+x a.out
mv a.out ~/binf77/mesh_bandwidth
#
echo "Executable installed as ~/binf77/mesh_bandwidth"
