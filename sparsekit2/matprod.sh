#!/bin/bash
#
gfortran -c matprod.f
if [ $? -ne 0 ]; then
  echo "Errors compiling matprod.f"
  exit
fi
#
gfortran matprod.o -L$HOME/libf77 -lsparsekit2
if [ $? -ne 0 ]; then
  echo "Errors linking and loading matprod.o"
  exit
fi
rm matprod.o
#
mv a.out matprod
./matprod > matprod_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running matprod"
  exit
fi
rm matprod
rm fort.9
#
echo "Test results written to matprod_output.txt."
