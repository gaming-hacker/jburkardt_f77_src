#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../toms661.f
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
ar qc libtoms661.a *.o
rm *.o
#
mv libtoms661.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms661.a"
