#! /bin/bash
#
gfortran -c -Wall toms597.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv toms597.o ~/libf77/toms597.o
#
echo "Normal end of execution."
