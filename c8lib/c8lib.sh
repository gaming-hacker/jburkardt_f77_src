#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../c8lib.f
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
ar qc libc8lib.a *.o
rm *.o
#
mv libc8lib.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libc8lib.a."
