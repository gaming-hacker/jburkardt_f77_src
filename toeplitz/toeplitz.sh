#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toeplitz.f
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
ar qc libtoeplitz.a *.o
rm *.o
#
mv libtoeplitz.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoeplitz.a."