#!/bin/bash
#
gfortran -c set_theory_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling set_theory_prb.f"
  exit
fi
#
gfortran set_theory_prb.o -L$HOME/libf77 -lset_theory
if [ $? -ne 0 ]; then
  echo "Errors linking and loading set_theory_prb.o"
  exit
fi
rm set_theory_prb.o
#
mv a.out set_theory_prb
./set_theory_prb > set_theory_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running set_theory_prb"
  exit
fi
rm set_theory_prb
#
echo "Test program output written to set_theory_prb_output.txt."
