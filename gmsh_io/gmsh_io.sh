#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../gmsh_io.f
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
ar qc libgmsh_io.a *.o
rm *.o
#
mv libgmsh_io.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libgmsh_io.a."
