#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../steam_nbs.f
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
ar qc libsteam_nbs.a *.o
rm *.o
#
mv libsteam_nbs.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsteam_nbs.a."
