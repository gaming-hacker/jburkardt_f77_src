#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../simplex_gm_rule.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libsimplex_gm_rule.a *.o
rm *.o
#
mv libsimplex_gm_rule.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsimplex_gm_rule.a"
