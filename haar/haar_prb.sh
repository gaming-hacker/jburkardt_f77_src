#!/bin/bash
#
gfortran -c haar_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling haar_prb.f"
  exit
fi
#
gfortran haar_prb.o -L$HOME/libf77 -lhaar
if [ $? -ne 0 ]; then
  echo "Errors linking and loading haar_prb.o"
  exit
fi
rm haar_prb.o
#
mv a.out haar_prb
./haar_prb > haar_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running haar_prb"
  exit
fi
rm haar_prb
#
echo "Test results written to haar_prb_output.txt."
