#!/bin/bash
#
gfortran -c lawson_prb4.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lawson_prb4.f"
  exit
fi
#
gfortran lawson_prb4.o -L$HOME/libf77 -llawson
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lawson_prb4.o"
  exit
fi
rm lawson_prb4.o
#
mv a.out lawson_prb4
./lawson_prb4 > lawson_prb4_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lawson_prb4"
  exit
fi
rm lawson_prb4
#
echo "Test results written to lawson_prb4_output.txt."
