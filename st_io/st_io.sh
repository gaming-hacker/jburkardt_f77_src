#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../st_io.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -g $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libst_io.a *.o
rm *.o
#
mv libst_io.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libst_io.a."
