#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../sort_rc.f
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
ar qc libsort_rc.a *.o
rm *.o
#
mv libsort_rc.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsort_rc.a"
