#! /bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../levinson_approx_1d.f
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
ar qc liblevinson_approx_1d.a *.o
rm *.o
#
mv liblevinson_approx_1d.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/liblevinson_approx_1d.a."
