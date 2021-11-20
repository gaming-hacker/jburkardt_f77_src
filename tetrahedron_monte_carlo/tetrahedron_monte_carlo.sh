#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../tetrahedron_monte_carlo.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f
#
ar qc libtetrahedron_monte_carlo.a *.o
rm *.o
#
mv libtetrahedron_monte_carlo.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libtetrahedron_monte_carlo.a"
