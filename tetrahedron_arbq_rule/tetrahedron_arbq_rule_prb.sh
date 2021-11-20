#!/bin/bash
#
gfortran -c tetrahedron_arbq_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tetrahedron_arbq_rule_prb.f"
  exit
fi
#
gfortran tetrahedron_arbq_rule_prb.o -L$HOME/libf77 -ltetrahedron_arbq_rule
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tetrahedron_arbq_rule_prb.o"
  exit
fi
rm tetrahedron_arbq_rule_prb.o
#
mv a.out tetrahedron_arbq_rule_prb
./tetrahedron_arbq_rule_prb > tetrahedron_arbq_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running tetrahedron_arbq_rule_prb"
  exit
fi
rm tetrahedron_arbq_rule_prb
#
echo "Test results written to tetrahedron_arbq_rule_prb_output.txt."
