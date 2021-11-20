#!/bin/bash
#
gfortran -c toms434_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms434_prb.f"
  exit
fi
#
gfortran toms434_prb.o -L$HOME/libf77 -ltoms434
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms434_prb.o"
  exit
fi
rm toms434_prb.o
#
mv a.out toms434_prb
./toms434_prb > toms434_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms434_prb"
  exit
fi
rm toms434_prb
#
echo "Test results written to toms434_prb_output.txt."
