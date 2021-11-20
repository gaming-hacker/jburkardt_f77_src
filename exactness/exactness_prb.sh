#!/bin/bash
#
gfortran -c exactness_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling exactness_prb.f"
  exit
fi
#
gfortran exactness_prb.o -L$HOME/libf77 -lexactness
if [ $? -ne 0 ]; then
  echo "Errors linking and loading exactness_prb.o"
  exit
fi
rm exactness_prb.o
#
mv a.out exactness_prb
./exactness_prb > exactness_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running exactness_prb"
  exit
fi
rm exactness_prb
#
echo "Test program output written to exactness_prb_output.txt."
