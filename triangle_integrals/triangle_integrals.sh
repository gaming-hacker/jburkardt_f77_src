#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../triangle_integrals.f
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
ar qc libtriangle_integrals.a *.o
rm *.o
#
mv libtriangle_integrals.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
