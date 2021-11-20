#!/bin/bash
#
gfortran -c st_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling st_io_prb.f"
  exit
fi
#
gfortran st_io_prb.o -L$HOME/libf77 -lst_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading st_io_prb.o"
  exit
fi
rm st_io_prb.o
#
mv a.out st_io_prb
./st_io_prb > st_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running st_io_prb"
  exit
fi
rm st_io_prb
#
echo "Test results written to st_io_prb_output.txt."
