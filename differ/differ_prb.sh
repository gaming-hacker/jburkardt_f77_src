#!/bin/bash
#
gfortran -c differ_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling differ_prb.f"
  exit
fi
#
gfortran differ_prb.o -L$HOME/libf77 -ldiffer
if [ $? -ne 0 ]; then
  echo "Errors linking and loading differ_prb.o"
  exit
fi
rm differ_prb.o
#
mv a.out differ_prb
./differ_prb > differ_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running differ_prb"
  exit
fi
rm differ_prb
#
echo "Test results written to differ_prb_output.txt."
