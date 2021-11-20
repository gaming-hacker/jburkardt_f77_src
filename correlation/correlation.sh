#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/Linux/f77split ../correlation.f
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
ar qc libcorrelation.a *.o
rm *.o
#
mv libcorrelation.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcorrelation.a"
