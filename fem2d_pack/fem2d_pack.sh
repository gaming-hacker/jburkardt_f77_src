#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../fem2d_pack.f
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
ar qc libfem2d_pack.a *.o
rm *.o
#
mv libfem2d_pack.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfem2d_pack.a."
