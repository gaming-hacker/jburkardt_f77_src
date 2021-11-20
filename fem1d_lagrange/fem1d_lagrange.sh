#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../fem1d_lagrange.f
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
ar qc libfem1d_lagrange.a *.o
rm *.o
#
mv libfem1d_lagrange.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfem1d_lagrange.a"
