#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../sparse_grid_cc.f
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
ar qc libsparse_grid_cc.a *.o
rm *.o
#
mv libsparse_grid_cc.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsparse_grid_cc.a"
