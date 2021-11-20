#!/bin/bash
#
gfortran -c qls_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling qls_prb.f"
  exit
fi
#
gfortran qls_prb.o -L$HOME/libf77 -lqls -lqr_solve
if [ $? -ne 0 ]; then
  echo "Errors linking and loading qls_prb.o"
  exit
fi
rm qls_prb.o
#
mv a.out qls_prb
./qls_prb > qls_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running qls_prb"
  exit
fi
rm qls_prb
#
echo "Test program output written to qls_prb_output.txt."
