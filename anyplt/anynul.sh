#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../anynul.f
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
ar qc libanynul.a *.o
rm *.o
#
mv libanynul.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libanynul.a"
