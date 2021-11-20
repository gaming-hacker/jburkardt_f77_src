#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../compass_search.f
#
for FILE in `ls -1 *.f`
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libcompass_search.a *.o
rm *.o
#
mv libcompass_search.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcompass_search.a"
