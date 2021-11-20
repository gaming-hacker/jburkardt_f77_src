#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../fd1d_bvp.f
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
ar qc libfd1d_bvp.a *.o
rm *.o
#
mv libfd1d_bvp.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfd1d_bvp.a."
