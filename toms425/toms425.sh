#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../src.f
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
ar qc libtoms425.a *.o
rm *.o
#
mv libtoms425.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms425.a."
