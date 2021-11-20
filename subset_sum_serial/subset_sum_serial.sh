#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../subset_sum_serial.f
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
ar qc libsubset_sum_serial.a *.o
rm *.o
#
mv libsubset_sum_serial.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsubset_sum_serial.a"
