#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../power_method.f
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
ar qc libpower_method.a *.o
rm *.o
#
mv libpower_method.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpower_method.a."
