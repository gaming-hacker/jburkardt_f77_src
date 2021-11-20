#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../rkf45.f
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
ar qc librkf45.a *.o
rm *.o
#
mv librkf45.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/librkf45.a."
