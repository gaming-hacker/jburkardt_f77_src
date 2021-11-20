#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../odepack.f
~/binc/f77split ../odepack_sub1.f
~/binc/f77split ../odepack_sub2.f
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
ar qc libodepack.a *.o
rm *.o
#
mv libodepack.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libodepack.a."
