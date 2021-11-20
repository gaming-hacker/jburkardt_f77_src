#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../qwv.f
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
ar qc libqwv.a *.o
rm *.o
#
mv libqwv.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libqwv.a"
