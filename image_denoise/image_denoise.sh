#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../image_denoise.f
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
ar qc libimage_denoise.a *.o
rm *.o
#
mv libimage_denoise.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libimage_denoise.a"
