#!/bin/bash
#
gfortran -c square_symq_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling square_symq_rule_prb.f"
  exit
fi
#
gfortran -o square_symq_rule_prb square_symq_rule_prb.o -L$HOME/libf77 -lsquare_symq_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading square_symq_rule_prb.o"
  exit
fi
rm square_symq_rule_prb.o
#
./square_symq_rule_prb > square_symq_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_symq_rule_prb"
  exit
fi
rm square_symq_rule_prb
#
echo "Test results written to square_symq_rule_prb_output.txt."
