#!/bin/bash
#
mkdir temp
cd temp
rm *
f77split ../pitcon66.f
f77split ../pitcon66_sub.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f
#
ar qc libpitcon66.a *.o
rm *.o
#
mv libpitcon66.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/libpitcon66.a"
