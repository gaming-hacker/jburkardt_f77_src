#!/bin/bash
#
gfortran -c ou_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ou_prb.f"
  exit
fi
#
gfortran ou_prb.o -L$HOME/libf77 -lou
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ou_prb.o"
  exit
fi
rm ou_prb.o
#
mv a.out ou_prb
./ou_prb > ou_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ou_prb"
  exit
fi
rm ou_prb
#
echo "Test program output written to ou_prb_output.txt."
