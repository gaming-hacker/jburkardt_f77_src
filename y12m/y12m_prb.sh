#!/bin/bash
#
gfortran -c -g y12m_prb.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling y12m_prb.f"
  exit
fi
rm compiler.txt
#
gfortran y12m_prb.o -L$HOME/libf77 -ly12m
if [ $? -ne 0 ]; then
  echo "Errors linking and loading y12m_prb.o"
  exit
fi
rm y12m_prb.o
#
mv a.out y12m_prb
./y12m_prb > y12m_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running y12m_prb"
  exit
fi
rm y12m_prb
#
echo "Test results written to y12m_prb_output.txt."
