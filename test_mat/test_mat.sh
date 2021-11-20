#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../test_mat.f
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
ar qc libtest_mat.a *.o
rm *.o
#
mv libtest_mat.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtest_mat.a."
