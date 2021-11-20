#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../hb_read.f
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
ar qc libhb_read.a *.o
rm *.o
#
mv libhb_read.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libhb_read.a"
