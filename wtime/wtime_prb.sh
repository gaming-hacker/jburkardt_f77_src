#!/bin/bash
#
gfortran -c wtime_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wtime_prb.f"
  exit
fi
#
gfortran wtime_prb.o -L$HOME/libf77 -lwtime
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wtime_prb.o"
  exit
fi
rm wtime_prb.o
#
mv a.out wtime_prb
./wtime_prb > wtime_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wtime_prb"
  exit
fi
rm wtime_prb
#
echo "Test results written to wtime_prb_output.txt."
