#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../wedge_integrals.f
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
ar qc libwedge_integrals.a *.o
rm *.o
#
mv libwedge_integrals.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libwedge_integrals.a"
