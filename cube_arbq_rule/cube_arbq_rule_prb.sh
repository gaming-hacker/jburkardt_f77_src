#!/bin/bash
#
gfortran -c cube_arbq_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cube_arbq_rule_prb.f"
  exit
fi
#
gfortran -o cube_arbq_rule_prb cube_arbq_rule_prb.o -L$HOME/libf77 -lcube_arbq_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cube_arbq_rule_prb.o"
  exit
fi
rm cube_arbq_rule_prb.o
#
./cube_arbq_rule_prb > cube_arbq_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cube_arbq_rule_prb"
  exit
fi
rm cube_arbq_rule_prb
#
echo "Test results written to cube_arbq_rule_prb_output.txt."
