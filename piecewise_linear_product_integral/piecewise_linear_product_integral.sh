#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../piecewise_linear_product_integral.f
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
ar qc libpiecewise_linear_product_integral.a *.o
rm *.o
#
mv libpiecewise_linear_product_integral.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpiecewise_linear_product_integral.a."
