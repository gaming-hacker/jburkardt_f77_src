#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../test_int_circle.f
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
ar qc libtest_int_circle.a *.o
rm *.o
#
mv libtest_int_circle.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtest_int_circle.a"
