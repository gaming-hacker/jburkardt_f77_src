#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../line_grid.f
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
ar qc libline_grid.a *.o
rm *.o
#
mv libline_grid.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libline_grid.a"
