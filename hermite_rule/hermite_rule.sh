#!/bin/bash
#
gfortran -c hermite_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hermite_rule.f"
  exit
fi
#
gfortran hermite_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hermite_rule.o"
  exit
fi
rm hermite_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/hermite_rule
#
echo "Executable installed as ~/binf77/hermite_rule"
