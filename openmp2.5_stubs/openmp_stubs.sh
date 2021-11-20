#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../openmp_stubs.f
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
ar qc libopenmp_stubs.a *.o
rm *.o
#
mv libopenmp_stubs.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libopenmp_stubs.a."
