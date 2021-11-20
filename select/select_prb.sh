#!/bin/bash
#
gfortran -c select_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling select_prb.f"
  exit
fi
#
gfortran select_prb.o -L$HOME/libf77 -lselect
if [ $? -ne 0 ]; then
  echo "Errors linking and loading select_prb.o"
  exit
fi
rm select_prb.o
#
mv a.out select_prb
./select_prb > select_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running select_prb"
  exit
fi
rm select_prb
#
echo "Test results written to select_prb_output.txt."
