#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../fem2d_bvp_quadratic.f
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
ar qc libfem2d_bvp_quadratic.a *.o
rm *.o
#
mv libfem2d_bvp_quadratic.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfem2d_bvp_quadratic.a"
