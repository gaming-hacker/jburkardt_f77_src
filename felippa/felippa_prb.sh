#!/bin/bash
#
gfortran -c felippa_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling felippa_prb.f"
  exit
fi
#
gfortran felippa_prb.o -L$HOME/libf77 -lfelippa
if [ $? -ne 0 ]; then
  echo "Errors linking and loading felippa_prb.o"
  exit
fi
rm felippa_prb.o
#
mv a.out felippa_prb
./felippa_prb > felippa_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running felippa_prb"
  exit
fi
rm felippa_prb
#
echo "Test program output written to felippa_prb_output.txt."
