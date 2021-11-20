#! /bin/bash
#
mkdir temp
cd temp
rm *
f77split ../hermite_integrands.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libhermite_integrands.a *.o
rm *.o
#
mv libhermite_integrands.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libhermite_integrands.a"
