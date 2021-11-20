#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toms418.f
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
ar qc libtoms418.a *.o
rm *.o
#
mv libtoms418.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtoms418.a."
