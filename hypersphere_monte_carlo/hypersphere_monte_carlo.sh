#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../hypersphere_monte_carlo.f
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
ar qc libhypersphere_monte_carlo.a *.o
rm *.o
#
mv libhypersphere_monte_carlo.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libhypersphere_monte_carlo.a"
