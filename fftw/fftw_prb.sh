#! /bin/bash
#
gfortran -c -I$HOME/include -fno-underscoring fftw_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fftw_prb.f."
  exit
fi
#
gfortran fftw_prb.o -lfftw3 -lm -lc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fftw_prb.o."
  exit
fi
#
rm fftw_prb.o
#
mv a.out fftw_prb
./fftw_prb > fftw_prb.txt
if [ $? -ne 0 ]; then
  echo "Errors running fftw_prb."
  exit
fi
rm fftw_prb
#
echo "Normal end of execution."
