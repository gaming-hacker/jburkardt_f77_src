#! /bin/bash
#
gfortran -c -Wall zero_rc.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv zero_rc.o ~/libf77/zero_rc.o
#
echo "Normal end of execution."
