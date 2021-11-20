#! /bin/bash
#
gfortran -c -Wall toms672.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv toms672.o ~/libf77/toms672.o
#
echo "Normal end of execution."
