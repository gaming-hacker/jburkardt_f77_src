#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../cycle_brent.f
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
ar qc libcycle_brent.a *.o
rm *.o
#
mv libcycle_brent.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcycle_brent.a"
