#!/bin/bash
#
gfortran -c legendre_rule_fast.f
if [ $? -ne 0 ]; then
  echo "Errors compiling legendre_rule_fast.f"
  exit
fi
#
gfortran legendre_rule_fast.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading legendre_rule_fast.o"
  exit
fi
rm legendre_rule_fast.o
#
chmod ugo+x a.out
mv a.out ~/binf77/legendre_rule_fast
#
echo "Executable installed as ~/binf77/legendre_rule_fast"
