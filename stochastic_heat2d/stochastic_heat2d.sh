#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../stochastic_heat2d.f
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
ar qc libstochastic_heat2d.a *.o
rm *.o
#
mv libstochastic_heat2d.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libstochastic_heat2d.a"
