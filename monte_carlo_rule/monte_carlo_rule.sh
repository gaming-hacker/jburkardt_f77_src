#!/bin/bash
#
gfortran -c monte_carlo_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling monte_carlo_rule.f"
  exit
fi
#
gfortran monte_carlo_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading monte_carlo_rule.o"
  exit
fi
rm monte_carlo_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/monte_carlo_rule
#
echo "Executable installed as ~/binf77/monte_carlo_rule"
