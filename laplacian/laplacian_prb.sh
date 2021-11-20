#!/bin/bash
#
gfortran -c laplacian_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling laplacian_prb.f"
  exit
fi
#
gfortran laplacian_prb.o -L$HOME/libf77 -llaplacian
if [ $? -ne 0 ]; then
  echo "Errors linking and loading laplacian_prb.o"
  exit
fi
rm laplacian_prb.o
#
mv a.out laplacian_prb
./laplacian_prb > laplacian_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running laplacian_prb"
  exit
fi
rm laplacian_prb
#
echo "Test results written to laplacian_prb_output.txt."
