#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../band_qr.f
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
ar qc libband_qr.a *.o
rm *.o
#
mv libband_qr.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libband_qr.a."
