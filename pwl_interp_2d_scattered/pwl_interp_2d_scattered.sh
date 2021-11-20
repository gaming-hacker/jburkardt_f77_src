#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../pwl_interp_2d_scattered.f
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
ar qc libpwl_interp_2d_scattered.a *.o
rm *.o
#
mv libpwl_interp_2d_scattered.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpwl_interp_2d_scattered.a"
