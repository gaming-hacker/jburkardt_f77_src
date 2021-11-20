#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../chebyshev_polynomial.f
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
ar qc libchebyshev_polynomial.a *.o
rm *.o
#
mv libchebyshev_polynomial.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libchebyshev_polynomial.a"
