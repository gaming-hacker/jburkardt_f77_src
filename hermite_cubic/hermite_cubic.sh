#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../hermite_cubic.f
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
ar qc libhermite_cubic.a *.o
rm *.o
#
mv libhermite_cubic.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libhermite_cubic.a"
