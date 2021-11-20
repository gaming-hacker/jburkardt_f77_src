#!/bin/bash
#
gfortran -c colnew_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling colnew_prb.f"
  exit
fi
#
gfortran colnew_prb.o -L$HOME/libf77 -lcolnew
if [ $? -ne 0 ]; then
  echo "Errors linking and loading colnew_prb.o"
  exit
fi
rm colnew_prb.o
#
mv a.out colnew_prb
./colnew_prb > colnew_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running colnew_prb"
  exit
fi
rm colnew_prb
#
echo "Test results written to colnew_prb_output.txt."
