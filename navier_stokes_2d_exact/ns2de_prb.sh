#!/bin/bash
#
gfortran -c ns2de_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ns2de_prb.f"
  exit
fi
#
gfortran -o ns2de_prb ns2de_prb.o -L$HOME/libf77 -lns2de
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ns2de_prb.o"
  exit
fi
rm ns2de_prb.o
#
./ns2de_prb > ns2de_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ns2de_prb"
  exit
fi
rm ns2de_prb
#
echo "Test program output written to ns2de_prb_output.txt."
