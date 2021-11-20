#! /bin/bash
#
gfortran -c -Wall nms_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o nms_test nms_test.o $HOME/libf77/nms.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm nms_test.o
#
./nms_test > nms_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm nms_test
#
echo "Normal end of execution."
