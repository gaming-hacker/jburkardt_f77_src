#!/bin/bash
#
gfortran -c mgmres_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mgmres_prb.f"
  exit
fi
#
gfortran mgmres_prb.o -L$HOME/libf77 -lmgmres
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mgmres_prb.o"
  exit
fi
rm mgmres_prb.o
#
mv a.out mgmres_prb
./mgmres_prb > mgmres_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running mgmres_prb"
  exit
fi
rm mgmres_prb
#
echo "Test results written to mgmres_prb_output.txt."
