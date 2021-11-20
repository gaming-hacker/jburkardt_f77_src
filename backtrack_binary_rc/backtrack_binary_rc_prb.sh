#!/bin/bash
#
gfortran -c backtrack_binary_rc_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling backtrack_binary_rc_prb.f"
  exit
fi
#
gfortran backtrack_binary_rc_prb.o -L$HOME/libf77 -lbacktrack_binary_rc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading backtrack_binary_rc_prb.o"
  exit
fi
rm backtrack_binary_rc_prb.o
#
mv a.out backtrack_binary_rc_prb
./backtrack_binary_rc_prb > backtrack_binary_rc_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running backtrack_binary_rc_prb"
  exit
fi
rm backtrack_binary_rc_prb
#
echo "Test program output written to backtrack_binary_rc_prb_output.txt."
