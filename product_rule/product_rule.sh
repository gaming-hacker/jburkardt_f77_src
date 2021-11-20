#!/bin/bash
#
gfortran -c product_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling product_rule.f"
  exit
fi
#
gfortran product_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading product_rule.o"
  exit
fi
rm product_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/product_rule
#
echo "Executable installed as ~/binf77/product_rule"
