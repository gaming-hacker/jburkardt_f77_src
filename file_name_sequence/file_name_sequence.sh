#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../file_name_sequence.f
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
ar qc libfile_name_sequence.a *.o
rm *.o
#
mv libfile_name_sequence.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libfile_name_sequence.a."
