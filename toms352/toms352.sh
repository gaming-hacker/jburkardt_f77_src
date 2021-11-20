#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toms352.f
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
ar qc libtoms352.a *.o
rm *.o
#
mv libtoms352.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms352.a."
