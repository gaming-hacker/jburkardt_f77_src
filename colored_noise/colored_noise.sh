#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../colored_noise.f
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
ar qc libcolored_noise.a *.o
rm *.o
#
mv libcolored_noise.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcolored_noise.a."
