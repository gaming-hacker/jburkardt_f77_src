#!/bin/bash
#
gfortran -c pyramid_rule.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_rule.f"
  exit
fi
#
gfortran pyramid_rule.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pyramid_rule.o"
  exit
fi
rm pyramid_rule.o
#
chmod ugo+x a.out
mv a.out ~/binf77/pyramid_rule
#
echo "Executable installed as ~/binf77/pyramid_rule"
