#!/bin/bash
#
gfortran -c tetrahedron_felippa_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tetrahedron_felippa_rule_prb.f"
  exit
fi
#
gfortran tetrahedron_felippa_rule_prb.o -L$HOME/libf77 -ltetrahedron_felippa_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tetrahedron_felippa_rule_prb.o"
  exit
fi
rm tetrahedron_felippa_rule_prb.o
#
mv a.out tetrahedron_felippa_rule_prb
./tetrahedron_felippa_rule_prb > tetrahedron_felippa_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running tetrahedron_felippa_rule_prb"
  exit
fi
rm tetrahedron_felippa_rule_prb
#
echo "Test program output written to tetrahedron_felippa_rule_prb_output.txt."
