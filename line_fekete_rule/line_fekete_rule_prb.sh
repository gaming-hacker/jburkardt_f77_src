#!/bin/bash
#
gfortran -c line_fekete_rule_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling line_fekete_rule_prb.f"
  exit
fi
#
gfortran line_fekete_rule_prb.o -L$HOME/libf77 -lline_fekete_rule -lqr_solve
if [ $? -ne 0 ]; then
  echo "Errors linking and loading line_fekete_rule_prb.o"
  exit
fi
rm line_fekete_rule_prb.o
#
mv a.out line_fekete_rule_prb
./line_fekete_rule_prb > line_fekete_rule_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running line_fekete_rule_prb"
  exit
fi
rm line_fekete_rule_prb
#
echo "Test program output written to line_fekete_rule_prb_output.txt."
