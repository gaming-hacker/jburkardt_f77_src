#! /bin/bash
#
gfortran -c -Wall zero.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv zero.o ~/libf77/zero.o
#
echo "Normal end of execution."
