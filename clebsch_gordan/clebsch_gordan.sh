#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../clebsch_gordan.f
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
ar qc libclebsch_gordan.a *.o
rm *.o
#
mv libclebsch_gordan.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libclebsch_gordan.a."
