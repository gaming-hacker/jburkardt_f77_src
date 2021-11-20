#! /bin/bash
#
gfortran -c -Wall nas.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o nas nas.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm nas.o
#
./nas > nas.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm nas
#
echo "Normal end of execution."
