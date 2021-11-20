#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../partition_problem.f
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
ar qc libpartition_problem.a *.o
rm *.o
#
mv libpartition_problem.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpartition_problem.a"
