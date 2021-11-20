#! /bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toms577.f
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
ar qc libtoms577.a *.o
rm *.o
#
mv libtoms577.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
