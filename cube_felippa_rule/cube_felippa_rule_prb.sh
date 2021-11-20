#!/bin/bash
#
gfortran -c cube_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cube_felippa_rule_prb.f"
  exit
fi
#
gfortran cube_felippa_rule_prb.o -L$HOME/libf77 -lcube_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cube_felippa_rule_prb.o"
  exit
fi
rm cube_felippa_rule_prb.o
#
mv a.out cube_felippa_rule_prb
./cube_felippa_rule_prb > cube_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cube_felippa_rule_prb"
  exit
fi
rm cube_felippa_rule_prb
#
echo "Test program output written to cube_felippa_rule_prb_output.txt."
