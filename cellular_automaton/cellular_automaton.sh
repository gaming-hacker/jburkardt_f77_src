#!/bin/bash
#
gfortran -c cellular_automaton.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cellular_automaton.f"
  exit
fi
#
gfortran cellular_automaton.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cellular_automaton.o"
  exit
fi
rm cellular_automaton.o
#
chmod ugo+x a.out
mv a.out ~/binf77/cellular_automaton
#
echo "Program installed as ~/binf77/cellular_automaton"
