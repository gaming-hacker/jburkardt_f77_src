#!/bin/bash
#
gfortran -c nspcg_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling nspcg_prb1.f"
  exit
fi
#
gfortran nspcg_prb1.o -L$HOME/libf77 -lnspcg
if [ $? -ne 0 ]; then
  echo "Errors linking and loading nspcg_prb1.o"
  exit
fi
rm nspcg_prb1.o
#
mv a.out nspcg_prb1
./nspcg_prb1 > nspcg_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running nspcg_prb1"
  exit
fi
rm nspcg_prb1
#
echo "Test results written to nspcg_prb1_output.txt."
