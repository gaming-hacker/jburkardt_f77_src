#!/bin/bash
#
gfortran -c svd_snowfall_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling svd_snowfall_prb.f"
  exit
fi
#
gfortran svd_snowfall_prb.o -L$HOME/libf77 -lsvd_snowfall
if [ $? -ne 0 ]; then
  echo "Errors linking and loading svd_snowfall_prb.o"
  exit
fi
rm svd_snowfall_prb.o
#
mv a.out svd_snowfall_prb
./svd_snowfall_prb > svd_snowfall_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running svd_snowfall_prb"
  exit
fi
rm svd_snowfall_prb
#
echo "Test results written to svd_snowfall_prb_output.txt."
