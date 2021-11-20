#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../fd1d_heat_steady.f
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
ar qc libfd1d_heat_steady.a *.o
rm *.o
#
mv libfd1d_heat_steady.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfd1d_heat_steady.a."