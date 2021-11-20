#!/bin/bash
#
gfortran -c dlap_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling dlap_io_prb.f"
  exit
fi
#
gfortran dlap_io_prb.o -L$HOME/libf77 -ldlap_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading dlap_io_prb.o"
  exit
fi
rm dlap_io_prb.o
#
mv a.out dlap_io_prb
./dlap_io_prb > dlap_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running dlap_io_prb"
  exit
fi
rm dlap_io_prb
#
echo "Test program output written to dlap_io_prb_output.txt."
