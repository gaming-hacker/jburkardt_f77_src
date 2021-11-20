#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../special_functions.f
#
for FILE in `ls -1 *.f`
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libspecial_functions.a *.o
rm *.o
#
mv libspecial_functions.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libspecial_functions.a"
