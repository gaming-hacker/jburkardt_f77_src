#!/bin/bash
#
gfortran -c ccn_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ccn_rule.f"
  exit
fi
#
gfortran ccn_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ccn_rule.o"
  exit
fi
rm ccn_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/ccn_rule
#
echo "Executable installed as ~/binf77/ccn_rule"
