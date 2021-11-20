#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../pce_ode_hermite.f
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
ar qc libpce_ode_hermite.a *.o
rm *.o
#
mv libpce_ode_hermite.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpce_ode_hermite.a"
