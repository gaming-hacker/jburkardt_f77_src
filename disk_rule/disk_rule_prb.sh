#!/bin/bash
#
gfortran -c disk_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling disk_rule_prb.f"
  exit
fi
#
gfortran disk_rule_prb.o -L$HOME/libf77 -ldisk_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading disk_rule_prb.o"
  exit
fi
rm disk_rule_prb.o
#
mv a.out disk_rule_prb
./disk_rule_prb > disk_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running disk_rule_prb"
  exit
fi
rm disk_rule_prb
#
echo "Test program output written to disk_rule_prb_output.txt."
