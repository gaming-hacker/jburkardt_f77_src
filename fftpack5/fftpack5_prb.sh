#!/bin/bash
#
gfortran -c fftpack5_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fftpack5_prb.f"
  exit
fi
#
gfortran fftpack5_prb.o -L$HOME/libf77 -lfftpack5
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fftpack5_prb.o"
  exit
fi
rm fftpack5_prb.o
#
mv a.out fftpack5_prb
./fftpack5_prb > fftpack5_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fftpack5_prb"
  exit
fi
rm fftpack5_prb
#
echo "Test results written to fftpack5_prb_output.txt."
