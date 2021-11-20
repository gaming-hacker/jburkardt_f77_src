#!/bin/bash
#
gfortran -c table_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling table_io_prb.f"
  exit
fi
#
gfortran table_io_prb.o -L$HOME/libf77 -ltable_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading table_io_prb.o"
  exit
fi
rm table_io_prb.o
#
mv a.out table_io_prb
./table_io_prb > table_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running table_io_prb"
  exit
fi
rm table_io_prb
#
echo "Test results written to table_io_prb_output.txt."
