#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../fem_io.f
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
ar qc libfem_io.a *.o
rm *.o
#
mv libfem_io.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfem_io.a"
