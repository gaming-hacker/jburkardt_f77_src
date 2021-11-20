#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../geometry.f
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
ar qc libgeometry.a *.o
rm *.o
#
mv libgeometry.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libgeometry.a."
