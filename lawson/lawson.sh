#!/bin/bash
#
mkdir temp
cd temp
rm *
~/binc/f77split ../lawson.f
#
foreach FILE (`ls -1 *.f`)
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
end
rm *.f
#
ar qc liblawson.a *.o
rm *.o
#
mv liblawson.a ~/libf77
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/liblawson.a."
