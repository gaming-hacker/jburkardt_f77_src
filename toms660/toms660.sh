#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../toms660.f
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
ar qc libtoms660.a *.o
rm *.o
#
mv libtoms660.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms660.a"
