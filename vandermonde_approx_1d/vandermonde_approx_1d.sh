#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../vandermonde_approx_1d.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -g $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libvandermonde_approx_1d.a *.o
rm *.o
#
mv libvandermonde_approx_1d.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libvandermonde_approx_1d.a"
