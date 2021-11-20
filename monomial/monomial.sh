#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../monomial.f
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
ar qc libmonomial.a *.o
rm *.o
#
mv libmonomial.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libmonomial.a"
