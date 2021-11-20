#!/bin/bash
#
gfortran -c square_arbq_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling square_arbq_rule_prb.f"
  exit
fi
#
gfortran square_arbq_rule_prb.o -L$HOME/libf77 -lsquare_arbq_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading square_arbq_rule_prb.o"
  exit
fi
rm square_arbq_rule_prb.o
#
mv a.out square_arbq_rule_prb
./square_arbq_rule_prb > square_arbq_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_arbq_rule_prb"
  exit
fi
rm square_arbq_rule_prb
#
echo "Test results written to square_arbq_rule_prb_output.txt."
