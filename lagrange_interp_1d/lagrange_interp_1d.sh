#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../lagrange_interp_1d.f
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
ar qc liblagrange_interp_1d.a *.o
rm *.o
#
mv liblagrange_interp_1d.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/liblagrange_interp_1d.a"
