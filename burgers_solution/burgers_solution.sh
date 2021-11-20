#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../burgers_solution.f
#
for FILE in `ls -1 *.f`
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libburgers_solution.a *.o
rm *.o
#
mv libburgers_solution.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libburgers_solution.a"
