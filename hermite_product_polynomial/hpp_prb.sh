#!/bin/bash
#
gfortran -c hpp_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hpp_prb.f"
  exit
fi
#
gfortran -o hpp_prb hpp_prb.o -L$HOME/libf77 -lhpp
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hpp_prb.o"
  exit
fi
rm hpp_prb.o
#
./hpp_prb > hpp_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hpp_prb"
  exit
fi
rm hpp_prb
#
echo "Test program output written to hpp_prb_output.txt."
