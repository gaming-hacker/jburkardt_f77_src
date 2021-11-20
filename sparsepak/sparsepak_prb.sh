#!/bin/bash
#
gfortran -c sparsepak_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsepak_prb.f"
  exit
fi
#
gfortran sparsepak_prb.o -L$HOME/libf77 -lsparsepak
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsepak_prb.o"
  exit
fi
rm sparsepak_prb.o
#
mv a.out sparsepak_prb
./sparsepak_prb > sparsepak_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsepak_prb"
  exit
fi
rm sparsepak_prb
#
echo "Test results written to sparsepak_prb_output.txt."
