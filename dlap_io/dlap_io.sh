#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../dlap_io.f
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
ar qc libdlap_io.a *.o
rm *.o
#
mv libdlap_io.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libdlap_io.a"
