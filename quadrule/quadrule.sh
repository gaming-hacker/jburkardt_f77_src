#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../quadrule.f
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
ar qc libquadrule.a *.o
rm *.o
#
mv libquadrule.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libquadrule.a."
