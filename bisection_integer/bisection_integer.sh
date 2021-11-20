#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../bisection_integer.f
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
ar qc libbisection_integer.a *.o
rm *.o
#
mv libbisection_integer.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libbisection_integer.a"
