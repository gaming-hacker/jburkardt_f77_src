#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../prime_serial.f
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
ar qc libprime_serial.a *.o
rm *.o
#
mv libprime_serial.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libprime_serial.a."
