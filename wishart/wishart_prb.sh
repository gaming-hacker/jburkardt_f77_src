#!/bin/bash
#
gfortran -c wishart_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wishart_prb.f"
  exit
fi
#
gfortran wishart_prb.o -L$HOME/libf77 -lwishart -lpdflib -lrnglib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wishart_prb.o"
  exit
fi
rm wishart_prb.o
#
mv a.out wishart_prb
./wishart_prb > wishart_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wishart_prb"
  exit
fi
rm wishart_prb
#
echo "Test program output written to wishart_prb_output.txt."
