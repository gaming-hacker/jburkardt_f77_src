#!/bin/bash
#
gfortran -c cnf_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cnf_io_prb.f"
  exit
fi
#
gfortran cnf_io_prb.o -L$HOME/libf77 -lcnf_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cnf_io_prb.o"
  exit
fi
rm cnf_io_prb.o
#
mv a.out cnf_io_prb
./cnf_io_prb > cnf_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cnf_io_prb"
  exit
fi
rm cnf_io_prb
#
echo "Test results written to cnf_io_prb_output.txt."
