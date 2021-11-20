#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../linpack_z.f
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
ar qc liblinpack_z.a *.o
rm *.o
#
mv liblinpack_z.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/liblinpack_z.a."
