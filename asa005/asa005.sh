#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../asa005.f
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
ar qc libasa005.a *.o
rm *.o
#
mv libasa005.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libasa005.a."
