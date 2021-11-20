#!/bin/bash
#
gfortran -c cc_to_st_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cc_to_st_prb.f"
  exit
fi
#
gfortran -o cc_to_st_prb cc_to_st_prb.o -L$HOME/libf77 -lcc_to_st
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cc_to_st_prb.o"
  exit
fi
rm cc_to_st_prb.o
#
./cc_to_st_prb > cc_to_st_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cc_to_st_prb"
  exit
fi
rm cc_to_st_prb
#
echo "Test program output written to cc_to_st_prb_output.txt."
