#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../glomin_original.f
~/binc/f77split ../localm_original.f
~/binc/f77split ../zero_original.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libbrent_original.a *.o
rm *.o
#
mv libbrent_original.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
