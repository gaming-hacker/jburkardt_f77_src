#!/bin/bash
#
gfortran -c legendre_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling legendre_rule.f"
  exit
fi
#
gfortran legendre_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading legendre_rule.o"
  exit
fi
rm legendre_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/legendre_rule
#
echo "Executable installed as ~/binf77/legendre_rule"
