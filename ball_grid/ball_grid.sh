#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../ball_grid.f
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
ar qc libball_grid.a *.o
rm *.o
#
mv libball_grid.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libball_grid.a"
