#!/bin/bash
#
gfortran -c padua_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling padua_prb.f"
  exit
fi
#
gfortran padua_prb.o -L$HOME/libf77 -lpadua
if [ $? -ne 0 ]; then
  echo "Errors linking and loading padua_prb.o"
  exit
fi
rm padua_prb.o
#
mv a.out padua_prb
./padua_prb > padua_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running padua_prb"
  exit
fi
rm padua_prb
#
echo "Test program output written to padua_prb_output.txt."
