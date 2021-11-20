#!/bin/bash
#
gfortran -c -g pitcon66_prb2.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling pitcon66_prb.f"
  exit
fi
rm compiler.txt
#
gfortran pitcon66_prb2.o -L$HOME/libf77 -lpitcon66
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pitcon66_prb.o"
  exit
fi
rm pitcon66_prb2.o
#
mv a.out pitcon66_prb2
./pitcon66_prb2 > pitcon66_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pitcon66_prb2"
  exit
fi
rm pitcon66_prb2
#
echo "Program output written to pitcon66_prb2_output.txt"
