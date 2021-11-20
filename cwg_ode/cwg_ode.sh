#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../cwg_ode.f
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
ar qc libcwg_ode.a *.o
rm *.o
#
mv libcwg_ode.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcwg_ode.a."
