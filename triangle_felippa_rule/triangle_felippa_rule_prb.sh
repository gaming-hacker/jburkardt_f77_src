#!/bin/bash
#
gfortran -c triangle_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle_felippa_rule_prb.f"
  exit
fi
#
gfortran triangle_felippa_rule_prb.o -L$HOME/libf77 -ltriangle_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle_felippa_rule_prb.o"
  exit
fi
rm triangle_felippa_rule_prb.o
#
mv a.out triangle_felippa_rule_prb
./triangle_felippa_rule_prb > triangle_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running triangle_felippa_rule_prb"
  exit
fi
rm triangle_felippa_rule_prb
#
echo "Test program output written to triangle_felippa_rule_prb_output.txt."
