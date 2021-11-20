#!/bin/bash
#
gfortran -c latin_cover_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling latin_cover_prb.f"
  exit
fi
#
gfortran latin_cover_prb.o -L$HOME/libf77 -llatin_cover
if [ $? -ne 0 ]; then
  echo "Errors linking and loading latin_cover_prb.o"
  exit
fi
rm latin_cover_prb.o
#
mv a.out latin_cover_prb
./latin_cover_prb > latin_cover_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running latin_cover_prb"
  exit
fi
rm latin_cover_prb
#
echo "Test program output written to latin_cover_prb_output.txt."
