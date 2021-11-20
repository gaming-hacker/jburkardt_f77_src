#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../tetrahedron_felippa_rule.f
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
ar qc libtetrahedron_felippa_rule.a *.o
rm *.o
#
mv libtetrahedron_felippa_rule.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtetrahedron_felippa_rule.a"
