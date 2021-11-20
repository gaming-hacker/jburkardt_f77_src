#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../toeplitz_cholesky.f
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
ar qc libtoeplitz_cholesky.a *.o
rm *.o
#
mv libtoeplitz_cholesky.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoeplitz_cholesky.a"
