#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../fd2d_heat_steady.f
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
ar qc libfd2d_heat_steady.a *.o
rm *.o
#
mv libfd2d_heat_steady.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfd2d_heat_steady.a"
