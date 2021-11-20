#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../matrix_exponential.f
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
ar qc libmatrix_exponential.a *.o
rm *.o
#
mv libmatrix_exponential.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libmatrix_exponential.a"
