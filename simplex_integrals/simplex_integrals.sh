#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../simplex_integrals.f
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
ar qc libsimplex_integrals.a *.o
rm *.o
#
mv libsimplex_integrals.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsimplex_integrals.a"
