#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../cube_exactness.f
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
ar qc libcube_exactness.a *.o
rm *.o
#
mv libcube_exactness.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcube_exactness.a"
