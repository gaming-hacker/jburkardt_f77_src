#!/bin/bash
#
gfortran -c fishpack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fishpack_prb.f"
  exit
fi
#
gfortran fishpack_prb.o -L$HOME/libf77 -lfishpack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fishpack_prb.o"
  exit
fi
rm fishpack_prb.o
#
mv a.out fishpack_prb
./fishpack_prb > fishpack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fishpack_prb"
  exit
fi
rm fishpack_prb
#
echo "Test results written to fishpack_prb_output.txt."
