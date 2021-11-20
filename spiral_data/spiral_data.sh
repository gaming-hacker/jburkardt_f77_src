#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../spiral_data.f
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
ar qc libspiral_data.a *.o
rm *.o
#
mv libspiral_data.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libspiral_data.a."
