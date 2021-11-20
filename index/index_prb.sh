#!/bin/bash
#
gfortran -c index_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling index_prb.f"
  exit
fi
#
gfortran index_prb.o -L$HOME/libf77 -lindex
if [ $? -ne 0 ]; then
  echo "Errors linking and loading index_prb.o"
  exit
fi
rm index_prb.o
#
mv a.out index_prb
./index_prb > index_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running index_prb"
  exit
fi
rm index_prb
#
echo "Test program output written to index_prb_output.txt."
