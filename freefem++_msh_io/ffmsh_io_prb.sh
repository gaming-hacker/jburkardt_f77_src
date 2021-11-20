#!/bin/bash
#
gfortran -c ffmsh_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ffmsh_io_prb.f"
  exit
fi
#
gfortran -o ffmsh_io_prb ffmsh_io_prb.o -L$HOME/libf77 -lffmsh_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ffmsh_io_prb.o"
  exit
fi
rm ffmsh_io_prb.o
#
./ffmsh_io_prb > ffmsh_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ffmsh_io_prb"
  exit
fi
rm ffmsh_io_prb
#
echo "Test program output written to ffmsh_io_prb_output.txt."
