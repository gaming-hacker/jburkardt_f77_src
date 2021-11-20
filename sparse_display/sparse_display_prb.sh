#!/bin/bash
#
gfortran -c sparse_display_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparse_display_prb.f"
  exit
fi
#
gfortran -o sparse_display_prb sparse_display_prb.o -L$HOME/libf77 -lsparse_display
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparse_display_prb.o"
  exit
fi
rm sparse_display_prb.o
#
./sparse_display_prb > sparse_display_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparse_display_prb"
  exit
fi
rm sparse_display_prb
#
echo "Test results written to sparse_display_prb_output.txt."
