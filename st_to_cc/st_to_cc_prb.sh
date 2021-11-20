#!/bin/bash
#
gfortran -c st_to_cc_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling st_to_cc_prb.f"
  exit
fi
#
gfortran -o st_to_cc_prb st_to_cc_prb.o -L$HOME/libf77 -lst_to_cc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading st_to_cc_prb.o"
  exit
fi
rm st_to_cc_prb.o
#
./st_to_cc_prb > st_to_cc_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running st_to_cc_prb"
  exit
fi
rm st_to_cc_prb
#
echo "Test program output written to st_to_cc_prb_output.txt."
