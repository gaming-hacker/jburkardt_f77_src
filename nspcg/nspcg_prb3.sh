#!/bin/bash
#
gfortran -c nspcg_prb3.f
if [ $? -ne 0 ]; then
  echo "Errors compiling nspcg_prb3.f"
  exit
fi
#
gfortran nspcg_prb3.o -L$HOME/libf77 -lnspcg
if [ $? -ne 0 ]; then
  echo "Errors linking and loading nspcg_prb3.o"
  exit
fi
rm nspcg_prb3.o
#
mv a.out nspcg_prb3
./nspcg_prb3 > nspcg_prb3_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running nspcg_prb3"
  exit
fi
rm nspcg_prb3
#
echo "Test results written to nspcg_prb3_output.txt."
