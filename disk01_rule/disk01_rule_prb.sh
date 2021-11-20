#! /bin/bash
#
gfortran -c disk01_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling disk01_rule_prb.f"
  exit
fi
#
gfortran disk01_rule_prb.o -L$HOME/libf77 -ldisk01_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading disk01_rule_prb.o"
  exit
fi
rm disk01_rule_prb.o
#
mv a.out disk01_rule_prb
./disk01_rule_prb > disk01_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running disk01_rule_prb"
  exit
fi
rm disk01_rule_prb
#
echo "Test program output written to disk01_rule_prb_output.txt."
