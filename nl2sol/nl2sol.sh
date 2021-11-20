#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../nl2sol.f
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
ar qc libnl2sol.a *.o
rm *.o
#
mv libnl2sol.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libnl2sol.a."
