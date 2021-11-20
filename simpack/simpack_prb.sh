#!/bin/bash
#
gfortran -c simpack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling simpack_prb.f"
  exit
fi
#
gfortran simpack_prb.o -L$HOME/libf77 -lsimpack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading simpack_prb.o"
  exit
fi
rm simpack_prb.o
#
mv a.out simpack_prb
./simpack_prb > simpack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running simpack_prb"
  exit
fi
rm simpack_prb
#
echo "Test results written to simpack_prb_output.txt."
