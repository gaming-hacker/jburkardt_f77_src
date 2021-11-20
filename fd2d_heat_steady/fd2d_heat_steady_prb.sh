#!/bin/bash
#
gfortran -c fd2d_heat_steady_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fd2d_heat_steady_prb.f"
  exit
fi
#
gfortran fd2d_heat_steady_prb.o -L$HOME/libf77 -lfd2d_heat_steady
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fd2d_heat_steady_prb.o"
  exit
fi
rm fd2d_heat_steady_prb.o
#
mv a.out fd2d_heat_steady_prb
./fd2d_heat_steady_prb > fd2d_heat_steady_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fd2d_heat_steady_prb"
  exit
fi
rm fd2d_heat_steady_prb
#
echo "Test program output written to fd2d_heat_steady_prb_output.txt."
