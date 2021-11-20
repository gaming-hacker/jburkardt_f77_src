#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../random_data.f
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
ar qc librandom_data.a *.o
rm *.o
#
mv librandom_data.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/librandom_data.a"
