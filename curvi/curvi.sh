#! /bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../rut.f
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
ar qc libcurvi.a *.o
rm *.o
#
mv libcurvi.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
