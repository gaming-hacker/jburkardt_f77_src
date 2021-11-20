#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../cc_to_st.f
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
ar qc libcc_to_st.a *.o
rm *.o
#
mv libcc_to_st.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libcc_to_st.a"
