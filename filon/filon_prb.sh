#!/bin/bash
#
gfortran -c filon_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling filon_prb.f"
  exit
fi
#
gfortran filon_prb.o -L$HOME/libf77 -lfilon
if [ $? -ne 0 ]; then
  echo "Errors linking and loading filon_prb.o"
  exit
fi
rm filon_prb.o
#
mv a.out filon_prb
./filon_prb > filon_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running filon_prb"
  exit
fi
rm filon_prb
#
echo "Test program output written to filon_prb_output.txt."
