#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../fem1d_bvp_linear.f
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
ar qc libfem1d_bvp_linear.a *.o
rm *.o
#
mv libfem1d_bvp_linear.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfem1d_bvp_linear.a"
