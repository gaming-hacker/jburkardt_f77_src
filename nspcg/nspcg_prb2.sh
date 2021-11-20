#!/bin/bash
#
gfortran -c nspcg_prb2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling nspcg_prb2.f"
  exit
fi
#
gfortran nspcg_prb2.o -L$HOME/libf77 -lnspcg
if [ $? -ne 0 ]; then
  echo "Errors linking and loading nspcg_prb2.o"
  exit
fi
rm nspcg_prb2.o
#
mv a.out nspcg_prb2
./nspcg_prb2 > nspcg_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running nspcg_prb2"
  exit
fi
rm nspcg_prb2
#
echo "Test results written to nspcg_prb2_output.txt."
