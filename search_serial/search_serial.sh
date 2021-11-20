#!/bin/bash
#
gfortran -c search_serial.f
if [ $? -ne 0 ]; then
  echo "Errors compiling search_serial.f"
  exit
fi
#
gfortran search_serial.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading search_serial.o"
  exit
fi
rm search_serial.o
#
mv a.out search_serial
./search_serial > search_serial_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running search_serial"
  exit
fi
rm search_serial
#
echo "Program output written to search_serial_output.txt"
