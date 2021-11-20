#!/bin/bash
#
gfortran -c spiral_data_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling spiral_data_prb.f"
  exit
fi
#
gfortran -o spiral_data_prb spiral_data_prb.o -L$HOME/libf77 -lspiral_data
if [ $? -ne 0 ]; then
  echo "Errors linking and loading spiral_data_prb.o"
  exit
fi
rm spiral_data_prb.o
#
./spiral_data_prb > spiral_data_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running spiral_data_prb"
  exit
fi
rm spiral_data_prb
#
echo "Test results written to spiral_data_prb_output.txt."
