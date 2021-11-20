#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../set_theory.f
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
ar qc libset_theory.a *.o
rm *.o
#
mv libset_theory.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libset_theory.a"
