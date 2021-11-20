#! /bin/bash
#
gfortran -c -Wall r8lib.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8lib.o ~/libf77/r8lib.o
#
echo "Normal end of execution."
