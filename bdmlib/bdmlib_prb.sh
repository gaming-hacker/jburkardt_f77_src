#!/bin/bash
#
gfortran -c bdmlib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bdmlib_prb.f"
  exit
fi
#
gfortran bdmlib_prb.o -L$HOME/libf77 -lbdmlib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bdmlib_prb.o"
  exit
fi
rm bdmlib_prb.o
#
mv a.out bdmlib_prb
./bdmlib_prb > bdmlib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bdmlib_prb"
  exit
fi
rm bdmlib_prb
#
echo "Test program output written to bdmlib_prb_output.txt."
