#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../cg_rc.f
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
ar qc libcg_rc.a *.o
rm *.o
#
mv libcg_rc.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcg_rc.a."
