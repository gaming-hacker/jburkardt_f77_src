#!/bin/bash
#
gfortran -c toms443_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms443_prb.f"
  exit
fi
#
gfortran toms443_prb.o -L$HOME/libf77 -ltoms443
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms443_prb.o"
  exit
fi
rm toms443_prb.o
#
mv a.out toms443_prb
./toms443_prb > toms443_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms443_prb"
  exit
fi
rm toms443_prb
#
echo "Test results written to toms443_prb_output.txt."
