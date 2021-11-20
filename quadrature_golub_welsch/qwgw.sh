#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../qwgw.f
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
ar qc libqwgw.a *.o
rm *.o
#
mv libqwgw.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libqwgw.a"
