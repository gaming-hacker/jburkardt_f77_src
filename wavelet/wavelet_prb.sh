#!/bin/bash
#
gfortran -c wavelet_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wavelet_prb.f"
  exit
fi
#
gfortran wavelet_prb.o -L$HOME/libf77 -lwavelet
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wavelet_prb.o"
  exit
fi
rm wavelet_prb.o
#
mv a.out wavelet_prb
./wavelet_prb > wavelet_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wavelet_prb"
  exit
fi
rm wavelet_prb
#
echo "Test program output written to wavelet_prb_output.txt."
