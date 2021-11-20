#!/bin/bash
#
gfortran -c subset_sum_serial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling subset_sum_serial_prb.f"
  exit
fi
#
gfortran subset_sum_serial_prb.o -L$HOME/libf77 -lsubset_sum_serial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading subset_sum_serial_prb.o"
  exit
fi
rm subset_sum_serial_prb.o
#
mv a.out subset_sum_serial_prb
./subset_sum_serial_prb > subset_sum_serial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running subset_sum_serial_prb"
  exit
fi
rm subset_sum_serial_prb
#
echo "Test program output written to subset_sum_serial_prb_output.txt."
