#!/bin/bash
#
gfortran -c square_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling square_felippa_rule_prb.f"
  exit
fi
#
gfortran square_felippa_rule_prb.o -L$HOME/libf77 -lsquare_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading square_felippa_rule_prb.o"
  exit
fi
rm square_felippa_rule_prb.o
#
mv a.out square_felippa_rule_prb
./square_felippa_rule_prb > square_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_felippa_rule_prb"
  exit
fi
rm square_felippa_rule_prb
#
echo "Test program output written to square_felippa_rule_prb_output.txt."
