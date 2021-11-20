#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../test_interp_fun.f
#
for FILE in `ls -1 *.f`
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
ar qc libtest_interp_fun.a *.o
rm *.o
#
mv libtest_interp_fun.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtest_interp_fun.a"
