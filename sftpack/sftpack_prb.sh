#!/bin/bash
#
gfortran -c sftpack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sftpack_prb.f"
  exit
fi
#
gfortran sftpack_prb.o -L$HOME/libf77 -lsftpack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sftpack_prb.o"
  exit
fi
rm sftpack_prb.o
#
mv a.out sftpack_prb
./sftpack_prb > sftpack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sftpack_prb"
  exit
fi
rm sftpack_prb
#
echo "Test results written to sftpack_prb_output.txt."
