#!/bin/bash
#
gfortran -c geyser.f
if [ $? -ne 0 ]; then
  echo "Errors compiling geyser.f"
  exit
fi
#
gfortran geyser.o -L/usr/local/dislin -ldislin -L/opt/local/lib -lXm
if [ $? -ne 0 ]; then
  echo "Errors linking and loading geyser.o."
  exit
fi
#
rm geyser.o
#
mv a.out geyser
./geyser > geyser_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running geyser."
  exit
fi
rm geyser
#
echo "Program output written to geyser_output.txt"
