#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../fftpack5.1.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -O3 $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar cr libfftpack5.1.a *.o
rm *.o
#
mv libfftpack5.1.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfftpack5.1.a."
