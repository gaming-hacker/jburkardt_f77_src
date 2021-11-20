#! /bin/bash
#
mkdir temp
cd temp
rm *
f77split ../hermite_interpolant.f
#
for FILE in `ls -1 *.f`
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f
#
ar qc libhermite_interpolant.a *.o
rm *.o
#
mv libhermite_interpolant.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
