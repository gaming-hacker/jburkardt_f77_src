#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../cc_io.f
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
ar qc libcc_io.a *.o
rm *.o
#
mv libcc_io.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcc_io.a"
