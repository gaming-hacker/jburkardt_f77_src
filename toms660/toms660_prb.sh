#!/bin/bash
#
gfortran -c toms660_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms660_prb.f"
  exit
fi
#
gfortran toms660_prb.o -L$HOME/libf77 -ltoms660
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms660_prb.o"
  exit
fi
rm toms660_prb.o
#
mv a.out toms660_prb
./toms660_prb > toms660_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms660_prb"
  exit
fi
rm toms660_prb
#
echo "Program output written to toms660_prb_output.txt"
