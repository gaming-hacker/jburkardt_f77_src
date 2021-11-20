#!/bin/bash
#
gfortran -c quadmom_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quadmom_prb.f"
  exit
fi
#
gfortran quadmom_prb.o -L$HOME/libf77 -lquadmom -ltoms655
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quadmom_prb.o"
  exit
fi
rm quadmom_prb.o
#
mv a.out quadmom_prb
./quadmom_prb > quadmom_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quadmom_prb"
  exit
fi
rm quadmom_prb
#
echo "Test program output written to quadmom_prb_output.txt."
