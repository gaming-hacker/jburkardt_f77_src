#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../tetrahedron_arbq_rule.f
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
ar qc libtetrahedron_arbq_rule.a *.o
rm *.o
#
mv libtetrahedron_arbq_rule.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtetrahedron_arbq_rule.a."
