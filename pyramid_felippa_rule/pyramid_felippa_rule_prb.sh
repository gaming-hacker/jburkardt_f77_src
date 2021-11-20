#!/bin/bash
#
gfortran -c pyramid_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_felippa_rule_prb.f"
  exit
fi
#
gfortran pyramid_felippa_rule_prb.o -L$HOME/libf77 -lpyramid_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pyramid_felippa_rule_prb.o"
  exit
fi
rm pyramid_felippa_rule_prb.o
#
mv a.out pyramid_felippa_rule_prb
./pyramid_felippa_rule_prb > pyramid_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pyramid_felippa_rule_prb"
  exit
fi
rm pyramid_felippa_rule_prb
#
echo "Test program output written to pyramid_felippa_rule_prb_output.txt."
