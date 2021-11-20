#!/bin/bash
#
gfortran -c compass_search_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling compass_search_prb.f"
  exit
fi
#
gfortran compass_search_prb.o -L$HOME/libf77 -lcompass_search
if [ $? -ne 0 ]; then
  echo "Errors linking and loading compass_search_prb.o"
  exit
fi
rm compass_search_prb.o
#
mv a.out compass_search_prb
./compass_search_prb > compass_search_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running compass_search_prb"
  exit
fi
rm compass_search_prb
#
echo "Program output written to compass_search_prb_output.txt"
