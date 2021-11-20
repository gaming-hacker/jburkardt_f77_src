#!/bin/bash
#
gfortran -c s2de_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling s2de_prb.f"
  exit
fi
#
gfortran -o s2de_prb s2de_prb.o -L$HOME/libf77 -ls2de
if [ $? -ne 0 ]; then
  echo "Errors linking and loading s2de_prb.o"
  exit
fi
rm s2de_prb.o
#
./s2de_prb > s2de_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running s2de_prb"
  exit
fi
rm s2de_prb
#
echo "Test program output written to s2de_prb_output.txt."
