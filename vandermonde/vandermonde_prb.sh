#!/bin/bash
#
gfortran -c vandermonde_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling vandermonde_prb.f"
  exit
fi
#
gfortran vandermonde_prb.o -L$HOME/libf77 -lvandermonde
if [ $? -ne 0 ]; then
  echo "Errors linking and loading vandermonde_prb.o"
  exit
fi
rm vandermonde_prb.o
#
mv a.out vandermonde_prb
./vandermonde_prb > vandermonde_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running vandermonde_prb"
  exit
fi
rm vandermonde_prb
#
echo "Test program output written to vandermonde_prb_output.txt."
