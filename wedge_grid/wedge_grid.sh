#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../wedge_grid.f
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
ar qc libwedge_grid.a *.o
rm *.o
#
mv libwedge_grid.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libwedge_grid.a"
