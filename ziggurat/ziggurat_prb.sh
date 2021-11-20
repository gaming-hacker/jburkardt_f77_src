#!/bin/bash
#
gfortran -c ziggurat_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ziggurat_prb.f"
  exit
fi
#
gfortran ziggurat_prb.o -L$HOME/libf77 -lziggurat
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ziggurat_prb.o"
  exit
fi
rm ziggurat_prb.o
#
mv a.out ziggurat_prb
./ziggurat_prb > ziggurat_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ziggurat_prb"
  exit
fi
rm ziggurat_prb
#
echo "Test results written to ziggurat_prb_output.txt."
