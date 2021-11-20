#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../sparse_interp_nd.f
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
ar qc libsparse_interp_nd.a *.o
rm *.o
#
mv libsparse_interp_nd.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libsparse_interp_nd.a"
