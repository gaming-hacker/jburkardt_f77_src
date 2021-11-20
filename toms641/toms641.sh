#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toms641.f
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
ar qc libtoms641.a *.o
rm *.o
#
mv libtoms641.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms641.a."
