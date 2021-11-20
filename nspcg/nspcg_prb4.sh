#!/bin/bash
#
gfortran -c nspcg_prb4.f
if [ $? -ne 0 ]; then
  echo "Errors compiling nspcg_prb4.f"
  exit
fi
#
gfortran nspcg_prb4.o -L$HOME/libf77 -lnspcg
if [ $? -ne 0 ]; then
  echo "Errors linking and loading nspcg_prb4.o"
  exit
fi
rm nspcg_prb4.o
#
mv a.out nspcg_prb4
./nspcg_prb4 > nspcg_prb4_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running nspcg_prb4"
  exit
fi
rm nspcg_prb4
#
echo "Test results written to nspcg_prb4_output.txt."
