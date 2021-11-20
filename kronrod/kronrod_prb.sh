#!/bin/bash
#
gfortran -c kronrod_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling kronrod_prb.f"
  exit
fi
#
gfortran kronrod_prb.o -L$HOME/libf77 -lkronrod
if [ $? -ne 0 ]; then
  echo "Errors linking and loading kronrod_prb.o"
  exit
fi
rm kronrod_prb.o
#
mv a.out kronrod_prb
./kronrod_prb > kronrod_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running kronrod_prb"
  exit
fi
rm kronrod_prb
#
echo "Test results written to kronrod_prb_output.txt."
