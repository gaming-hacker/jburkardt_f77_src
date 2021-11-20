#!/bin/bash
#
gfortran -c simplex_gm_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling simplex_gm_rule_prb.f"
  exit
fi
#
gfortran simplex_gm_rule_prb.o -L$HOME/libf77 -lsimplex_gm_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading simplex_gm_rule_prb.o"
  exit
fi
rm simplex_gm_rule_prb.o
#
mv a.out simplex_gm_rule_prb
./simplex_gm_rule_prb > simplex_gm_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running simplex_gm_rule_prb"
  exit
fi
rm simplex_gm_rule_prb
#
echo "Test program output written to simplex_gm_rule_prb_output.txt."
