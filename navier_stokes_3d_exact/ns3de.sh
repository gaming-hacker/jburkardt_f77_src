#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../ns3de.f
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
ar qc libns3de.a *.o
rm *.o
#
mv libns3de.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libns3de.a"
