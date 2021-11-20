#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../circle_segment.f
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
ar qc libcircle_segment.a *.o
rm *.o
#
mv libcircle_segment.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcircle_segment.a"
