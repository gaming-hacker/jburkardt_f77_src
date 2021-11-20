#! /bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../toms449.f
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
ar qc libtoms449.a *.o
rm *.o
#
mv libtoms449.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
