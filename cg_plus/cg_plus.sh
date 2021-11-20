#! /bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../cg_plus.f
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
ar qc libcg_plus.a *.o
rm *.o
#
mv libcg_plus.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
