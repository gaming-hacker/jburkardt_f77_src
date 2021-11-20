#! /bin/bash
#
gfortran -c -Wall local_min.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv local_min.o ~/libf77/local_min.o
#
echo "Normal end of execution."
