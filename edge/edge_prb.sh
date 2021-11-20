#!/bin/bash
#
gfortran -c edge_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling edge_prb.f"
  exit
fi
#
gfortran edge_prb.o -L$HOME/libf77 -ledge
if [ $? -ne 0 ]; then
  echo "Errors linking and loading edge_prb.o"
  exit
fi
rm edge_prb.o
#
mv a.out edge_prb
./edge_prb > edge_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running edge_prb"
  exit
fi
rm edge_prb
#
echo "Test program output written to edge_prb_output.txt."
