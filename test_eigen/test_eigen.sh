#! /bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../test_eigen.f
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
ar qc libtest_eigen.a *.o
rm *.o
#
mv libtest_eigen.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
