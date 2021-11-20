#!/bin/bash
#
gfortran -c fem1d_pack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem1d_pack_prb.f"
  exit
fi
#
gfortran fem1d_pack_prb.o -L$HOME/libf77 -lfem1d_pack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_pack_prb.o"
  exit
fi
rm fem1d_pack_prb.o
#
mv a.out fem1d_pack_prb
./fem1d_pack_prb > fem1d_pack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fem1d_pack_prb"
  exit
fi
rm fem1d_pack_prb
#
echo "Test program output written to fem1d_pack_prb_output.txt."
