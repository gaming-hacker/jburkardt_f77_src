#!/bin/bash
#
gfortran -c -g y12m_prb2.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling y12m_prb2.f"
  exit
fi
rm compiler.txt
#
gfortran y12m_prb2.o -L$HOME/libf77 -ly12m
if [ $? -ne 0 ]; then
  echo "Errors linking and loading y12m_prb2.o"
  exit
fi
rm y12m_prb2.o
#
mv a.out y12m_prb2
./y12m_prb2 > y12m_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running y12m_prb2"
  exit
fi
rm y12m_prb2
#
echo "Test results written to y12m_prb2_output.txt."
