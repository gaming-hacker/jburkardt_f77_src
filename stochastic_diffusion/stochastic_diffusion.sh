#!/bin/bash
#
mkdir temp
cd temp
rm *
~/bin/f90split ../stochastic_diffusion.f
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
ar qc libstochastic_diffusion.a *.o
rm *.o
#
mv libstochastic_diffusion.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libstochastic_diffusion.a"
