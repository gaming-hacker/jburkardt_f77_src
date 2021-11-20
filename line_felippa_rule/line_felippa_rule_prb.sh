#!/bin/bash
#
gfortran -c line_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling line_felippa_rule_prb.f"
  exit
fi
#
gfortran line_felippa_rule_prb.o -L$HOME/libf77 -lline_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading line_felippa_rule_prb.o"
  exit
fi
rm line_felippa_rule_prb.o
#
mv a.out line_felippa_rule_prb
./line_felippa_rule_prb > line_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running line_felippa_rule_prb"
  exit
fi
rm line_felippa_rule_prb
#
echo "Test program output written to line_felippa_rule_prb_output.txt."
