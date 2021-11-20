#!/bin/bash
#
gfortran -c mgrid.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mgrid.f"
  exit
fi
#
gfortran mgrid.o -L$HOME/libf77 -lnapack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mgrid.o"
  exit
fi
rm mgrid.o
#
mv a.out mgrid
./mgrid > mgrid_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running mgrid"
  exit
fi
rm mgrid
#
echo "Program output written to mgrid_output.txt"
