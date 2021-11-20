#! /bin/bash
#
gfortran -c -Wall nms.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv nms.o ~/libf77/nms.o
#
echo "Normal end of execution."
