#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../triangle_symq_rule.f
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
ar qc libtriangle_symq_rule.a *.o
rm *.o
#
mv libtriangle_symq_rule.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtriangle_symq_rule.a."
