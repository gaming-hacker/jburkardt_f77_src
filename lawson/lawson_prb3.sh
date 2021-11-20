#!/bin/bash
#
gfortran -c lawson_prb3.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lawson_prb3.f"
  exit
fi
#
gfortran lawson_prb3.o -L$HOME/libf77 -llawson
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lawson_prb3.o"
  exit
fi
rm lawson_prb3.o
#
mv a.out lawson_prb3
./lawson_prb3 > lawson_prb3_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lawson_prb3"
  exit
fi
rm lawson_prb3
#
echo "Test results written to lawson_prb3_output.txt."
