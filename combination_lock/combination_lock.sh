#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../combination_lock.f
#
for FILE in `ls -1 *.f`
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libcombination_lock.a *.o
rm *.o
#
mv libcombination_lock.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcombination_lock.a"
