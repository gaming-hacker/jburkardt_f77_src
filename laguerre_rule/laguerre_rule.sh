#!/bin/bash
#
gfortran -c laguerre_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling laguerre_rule.f"
  exit
fi
#
gfortran laguerre_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading laguerre_rule.o"
  exit
fi
rm laguerre_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/laguerre_rule
#
echo "Executable installed as ~/binf77/laguerre_rule"
