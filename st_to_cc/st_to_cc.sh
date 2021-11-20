#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../st_to_cc.f
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
ar qc libst_to_cc.a *.o
rm *.o
#
mv libst_to_cc.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libst_to_cc.a"
