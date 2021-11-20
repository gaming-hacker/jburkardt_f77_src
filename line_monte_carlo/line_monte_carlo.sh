#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../line_monte_carlo.f
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
ar qc libline_monte_carlo.a *.o
rm *.o
#
mv libline_monte_carlo.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libline_monte_carlo.a."
