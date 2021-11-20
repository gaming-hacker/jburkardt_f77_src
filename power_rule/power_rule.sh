#!/bin/bash
#
gfortran -c power_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling power_rule.f"
  exit
fi
#
gfortran power_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading power_rule.o"
  exit
fi
rm power_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/power_rule
#
echo "Executable installed as ~/binf77/power_rule"
