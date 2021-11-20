#!/bin/bash
#
cp mpi_stubs_f77.h ~/include/mpi_stubs_f77.h
#
mkdir temp
cd temp
~/binc/f77split ../mpi_stubs.f
cp ../mpi_stubs_f77.h .
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -g $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
rm mpi_stubs_f77.h
#
ar qc libmpi_stubs.a *.o
rm *.o
#
mv libmpi_stubs.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libmpi_stubs.a."
