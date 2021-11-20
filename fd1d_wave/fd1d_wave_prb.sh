#!/bin/bash
#
gfortran -c fd1d_wave_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd1d_wave_prb.f"
  exit
fi
#
gfortran fd1d_wave_prb.o -L$HOME/libf77 -lfd1d_wave
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fd1d_wave_prb.o"
  exit
fi
rm fd1d_wave_prb.o
#
mv a.out fd1d_wave_prb
./fd1d_wave_prb > fd1d_wave_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fd1d_wave_prb"
  exit
fi
rm fd1d_wave_prb
#
echo "Test program output written to fd1d_wave_prb_output.txt."
