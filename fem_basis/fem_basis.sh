#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../fem_basis.f
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
ar qc libfem_basis.a *.o
rm *.o
#
mv libfem_basis.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfem_basis.a"
