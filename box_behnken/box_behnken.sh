#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../box_behnken.f
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
ar qc libbox_behnken.a *.o
rm *.o
#
mv libbox_behnken.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libbox_behnken.a."
