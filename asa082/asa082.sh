#! /bin/bash
#
gfortran -c -Wall asa082.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa082.o ~/libf77/asa082.o
#
echo "Normal end of execution."
