#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../pgma_io.f
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
ar qc libpgma_io.a *.o
rm *.o
#
mv libpgma_io.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpgma_io.a."
