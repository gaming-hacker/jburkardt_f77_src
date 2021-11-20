#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../y12m.f
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
ar qc liby12m.a *.o
rm *.o
#
mv liby12m.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/liby12m.a."
