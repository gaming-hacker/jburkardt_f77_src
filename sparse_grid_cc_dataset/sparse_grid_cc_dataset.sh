#!/bin/bash
#
gfortran -c sparse_grid_cc_dataset.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparse_grid_cc_dataset.f"
  exit
fi
#
gfortran sparse_grid_cc_dataset.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparse_grid_cc_dataset.o"
  exit
fi
rm sparse_grid_cc_dataset.o
#
chmod ugo+x a.out
mv a.out ~/binf77/sparse_grid_cc_dataset
#
echo "Executable installed as ~/binf77/sparse_grid_cc_dataset"
