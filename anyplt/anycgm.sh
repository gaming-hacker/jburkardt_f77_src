#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../anycgm.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f
#
ar qc libanycgm.a *.o
rm *.o
#
mv libanycgm.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libanycgm.a"
