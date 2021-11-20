#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../triangle01_integrals.f
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
ar qc libtriangle01_integrals.a *.o
rm *.o
#
mv libtriangle01_integrals.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtriangle01_integrals.a"
