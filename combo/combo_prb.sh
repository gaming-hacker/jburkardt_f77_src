#!/bin/bash
#
gfortran -c combo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling combo_prb.f"
  exit
fi
#
gfortran combo_prb.o -L$HOME/libf77 -lcombo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading combo_prb.o"
  exit
fi
rm combo_prb.o
#
mv a.out combo_prb
./combo_prb > combo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running combo_prb"
  exit
fi
rm combo_prb
#
echo "Test program output written to combo_prb_output.txt."
