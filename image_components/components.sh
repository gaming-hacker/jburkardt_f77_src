#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../components.f
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
ar qc libcomponents.a *.o
rm *.o
#
mv libcomponents.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcomponents.a"
