#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../cube_arbq_rule.f
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
ar qc libcube_arbq_rule.a *.o
rm *.o
#
mv libcube_arbq_rule.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcube_arbq_rule.a."
