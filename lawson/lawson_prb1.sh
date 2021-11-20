#!/bin/bash
#
gfortran -c lawson_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lawson_prb1.f"
  exit
fi
#
gfortran lawson_prb1.o -L$HOME/libf77 -llawson
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lawson_prb1.o"
  exit
fi
rm lawson_prb1.o
#
mv a.out lawson_prb1
./lawson_prb1 > lawson_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lawson_prb1"
  exit
fi
rm lawson_prb1
#
echo "Test results written to lawson_prb1_output.txt."
