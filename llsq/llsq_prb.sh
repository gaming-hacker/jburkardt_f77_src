#!/bin/bash
#
gfortran -c llsq_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling llsq_prb.f"
  exit
fi
#
gfortran llsq_prb.o -L$HOME/libf77 -lllsq
if [ $? -ne 0 ]; then
  echo "Errors linking and loading llsq_prb.o"
  exit
fi
rm llsq_prb.o
#
mv a.out llsq_prb
./llsq_prb > llsq_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running llsq_prb"
  exit
fi
rm llsq_prb
#
echo "Test program output written to llsq_prb_output.txt."
