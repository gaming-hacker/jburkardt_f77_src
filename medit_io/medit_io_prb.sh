#!/bin/bash
#
gfortran -c medit_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling medit_io_prb.f"
  exit
fi
#
gfortran medit_io_prb.o -L$HOME/libf77 -lmedit_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading medit_io_prb.o"
  exit
fi
rm medit_io_prb.o
#
mv a.out medit_io_prb
./medit_io_prb > medit_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running medit_io_prb"
  exit
fi
rm medit_io_prb
#
echo "Test program output written to medit_io_prb_output.txt."
