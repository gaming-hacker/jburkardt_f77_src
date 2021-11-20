#!/bin/bash
#
gfortran -c life_serial.f
if [ $? -ne 0 ]; then
  echo "Errors compiling life_serial.f"
  exit
fi
#
gfortran life_serial.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading life_serial.o"
  exit
fi
rm life_serial.o
#
mv a.out life_serial
./life_serial > life_serial_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running life_serial"
  exit
fi
rm life_serial
#
echo "Program output written to life_serial_output.txt"
