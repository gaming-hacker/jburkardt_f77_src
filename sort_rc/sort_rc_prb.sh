#!/bin/bash
#
gfortran -c sort_rc_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sort_rc_prb.f"
  exit
fi
#
gfortran -o sort_rc_prb sort_rc_prb.o -L$HOME/libf77 -lsort_rc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sort_rc_prb.o"
  exit
fi
rm sort_rc_prb.o
#
./sort_rc_prb > sort_rc_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sort_rc_prb"
  exit
fi
rm sort_rc_prb
#
echo "Test program output written to sort_rc_prb_output.txt."
