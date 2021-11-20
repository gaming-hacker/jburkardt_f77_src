#!/bin/bash
#
gfortran -c clenshaw_curtis_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling clenshaw_curtis_rule.f"
  exit
fi
#
gfortran clenshaw_curtis_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading clenshaw_curtis_rule.o"
  exit
fi
rm clenshaw_curtis_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/clenshaw_curtis_rule
#
echo "Executable installed as ~/binf77/clenshaw_curtis_rule"
