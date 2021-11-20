#! /bin/bash
#
gfortran -c -Wall local_min_rc.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv local_min_rc.o ~/libf77/local_min_rc.o
#
echo "Normal end of execution."
