#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../sphere_grid.f
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
ar qc libsphere_grid.a *.o
rm *.o
#
mv libsphere_grid.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsphere_grid.a"
