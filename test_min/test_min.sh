#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../test_min.f
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
ar qc libtest_min.a *.o
rm *.o
#
mv libtest_min.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtest_min.a"
