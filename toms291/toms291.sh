#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toms291.f
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
ar qc libtoms291.a *.o
rm *.o
#
mv libtoms291.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms291.a."
