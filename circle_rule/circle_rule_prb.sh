#!/bin/bash
#
gfortran -c circle_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling circle_rule_prb.f"
  exit
fi
#
gfortran circle_rule_prb.o -L$HOME/libf77 -lcircle_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading circle_rule_prb.o"
  exit
fi
rm circle_rule_prb.o
#
mv a.out circle_rule_prb
./circle_rule_prb > circle_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running circle_rule_prb"
  exit
fi
rm circle_rule_prb
#
echo "Test program output written to circle_rule_prb_output.txt."
