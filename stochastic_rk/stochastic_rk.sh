#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../stochastic_rk.f
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
ar qc libstochastic_rk.a *.o
rm *.o
#
mv libstochastic_rk.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libstochastic_rk.a."
