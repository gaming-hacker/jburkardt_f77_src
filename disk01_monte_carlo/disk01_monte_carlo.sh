#! /bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../disk01_monte_carlo.f
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
ar qc libdisk01_monte_carlo.a *.o
rm *.o
#
mv libdisk01_monte_carlo.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
