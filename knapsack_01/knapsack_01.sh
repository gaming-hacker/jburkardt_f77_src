#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../knapsack_01.f
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
ar qc libknapsack_01.a *.o
rm *.o
#
mv libknapsack_01.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libknapsack_01.a"
