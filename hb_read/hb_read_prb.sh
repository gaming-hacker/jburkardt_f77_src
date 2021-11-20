#!/bin/bash
#
gfortran -c hb_read_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hb_read_prb.f"
  exit
fi
#
gfortran hb_read_prb.o -L$HOME/libf77 -lhb_read
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hb_read_prb.o"
  exit
fi
rm hb_read_prb.o
#
mv a.out hb_read_prb
./hb_read_prb > hb_read_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hb_read_prb"
  exit
fi
rm hb_read_prb
#
echo "Test program output written to hb_read_prb_output.txt."
