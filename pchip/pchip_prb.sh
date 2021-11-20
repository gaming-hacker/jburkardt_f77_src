#!/bin/bash
#
gfortran -c pchip_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pchip_prb.f"
  exit
fi
#
gfortran -o pchip_prb pchip_prb.o -L$HOME/libf77 -lpchip
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pchip_prb.o"
  exit
fi
rm pchip_prb.o
#
./pchip_prb > pchip_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pchip_prb"
  exit
fi
rm pchip_prb
#
echo "Test results written to pchip_prb_output.txt."
