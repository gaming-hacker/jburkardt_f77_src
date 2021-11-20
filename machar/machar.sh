#! /bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../machar.f
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
ar qc libmachar.a *.o
rm *.o
#
mv libmachar.a ~/libf77/libmachar.a
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libmachar.a."
