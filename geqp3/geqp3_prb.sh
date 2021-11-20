#!/bin/bash
#
gfortran -c geqp3_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling geqp3_prb.f"
  exit
fi
#
gfortran geqp3_prb.o -L$HOME/libf77 -lgeqp3
if [ $? -ne 0 ]; then
  echo "Errors linking and loading geqp3_prb.o"
  exit
fi
rm geqp3_prb.o
#
mv a.out geqp3_prb
./geqp3_prb > geqp3_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running geqp3_prb"
  exit
fi
rm geqp3_prb
#
echo "Test results written to geqp3_prb_output.txt."
