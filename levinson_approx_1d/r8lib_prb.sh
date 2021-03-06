#! /bin/bash
#
gfortran -c r8lib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling r8lib_prb.f"
  exit
fi
#
gfortran -o r8lib_prb r8lib_prb.o -L$HOME/libf77 -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading r8lib_prb.o"
  exit
fi
rm r8lib_prb.o
#
./r8lib_prb > r8lib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running r8lib_prb"
  exit
fi
rm r8lib_prb
#
echo "Output written to r8lib_prb_output.txt."
