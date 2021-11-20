#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../high_card_simulation.f
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
ar qc libhigh_card_simulation.a *.o
rm *.o
#
mv libhigh_card_simulation.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libhigh_card_simulation.a"
