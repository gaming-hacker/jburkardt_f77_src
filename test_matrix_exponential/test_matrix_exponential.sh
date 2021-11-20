#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../test_matrix_exponential.f
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
ar qc libtest_matrix_exponential.a *.o
rm *.o
#
mv libtest_matrix_exponential.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtest_matrix_exponential.a"
