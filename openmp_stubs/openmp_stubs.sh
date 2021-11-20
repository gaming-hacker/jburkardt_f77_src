#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../openmp_stubs.f
#
cp ../omp_lib_kinds.h .
cp ../omp_lib.h .
for FILE in `ls -1 *.f`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
rm *.h
#
ar qc libopenmp_stubs.a *.o
rm *.o
#
mv libopenmp_stubs.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libopenmp_stubs.a."
