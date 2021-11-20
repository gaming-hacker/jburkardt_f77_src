#!/bin/bash
#
gfortran -c wedge_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wedge_felippa_rule_prb.f"
  exit
fi
#
gfortran wedge_felippa_rule_prb.o -L$HOME/libf77 -lwedge_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wedge_felippa_rule_prb.o"
  exit
fi
rm wedge_felippa_rule_prb.o
#
mv a.out wedge_felippa_rule_prb
./wedge_felippa_rule_prb > wedge_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wedge_felippa_rule_prb"
  exit
fi
rm wedge_felippa_rule_prb
#
echo "Test program output written to wedge_felippa_rule_prb_output.txt."
