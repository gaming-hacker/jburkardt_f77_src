#! /bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../elliptic_integral.f
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
ar qc libelliptic_integral.a *.o
rm *.o
#
mv libelliptic_integral.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
