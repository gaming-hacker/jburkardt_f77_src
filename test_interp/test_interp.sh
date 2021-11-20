#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../test_interp.f
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
ar qc libtest_interp.a *.o
rm *.o
#
mv libtest_interp.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtest_interp.a"