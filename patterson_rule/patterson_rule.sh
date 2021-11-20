#!/bin/bash
#
gfortran -c patterson_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling patterson_rule.f"
  exit
fi
#
gfortran patterson_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading patterson_rule.o"
  exit
fi
rm patterson_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/patterson_rule
#
echo "Executable installed as ~/binf77/patterson_rule"
