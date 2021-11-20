#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../image_edge.f
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
ar qc libimage_edge.a *.o
rm *.o
#
mv libimage_edge.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libimage_edge.a."
