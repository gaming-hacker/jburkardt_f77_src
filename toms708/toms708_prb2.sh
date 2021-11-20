#!/bin/bash
#
gfortran -c toms708_prb2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms708_prb2.f"
  exit
fi
#
gfortran toms708_prb2.o -L$HOME/libf77 -ltoms708
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms708_prb2.o"
  exit
fi
rm toms708_prb2.o
#
mv a.out toms708_prb2
./toms708_prb2 > toms708_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms708_prb2"
  exit
fi
rm toms708_prb2
#
echo "Test results written to toms708_prb2_output.txt."
