#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../poisson_simulation.f
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
ar qc libpoisson_simulation.a *.o
rm *.o
#
mv libpoisson_simulation.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpoisson_simulation.a"
