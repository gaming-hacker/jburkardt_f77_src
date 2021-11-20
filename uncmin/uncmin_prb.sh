#!/bin/bash
#
gfortran -c uncmin_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling uncmin_prb.f"
  exit
fi
#
gfortran uncmin_prb.o -L$HOME/libf77 -luncmin
if [ $? -ne 0 ]; then
  echo "Errors linking and loading uncmin_prb.o"
  exit
fi
rm uncmin_prb.o
#
mv a.out uncmin_prb
./uncmin_prb > uncmin_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running uncmin_prb"
  exit
fi
rm uncmin_prb
#
echo "Test results written to uncmin_prb_output.txt."
