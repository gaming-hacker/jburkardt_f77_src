#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../asa314.f
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
ar qc libasa314.a *.o
rm *.o
#
mv libasa314.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libasa314.a."
