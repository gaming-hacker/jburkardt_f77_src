#! /bin/bash
#
gfortran -c -Wall glomin.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv glomin.o ~/libf77/glomin.o
#
echo "Normal end of execution."
