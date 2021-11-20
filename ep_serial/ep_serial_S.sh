#!/bin/bash
#
cp npbparams_S.h npbparams.h
#
gfortran -c ep_serial.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ep_serial.f"
  exit
fi
#
gcc -c wtime.c
if [ $? -ne 0 ]; then
  echo "Errors compiling wtime.c"
  exit
fi
#
gfortran ep_serial.o wtime.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ep_serial.o + wtime.o"
  exit
fi
rm ep_serial.o
rm wtime.o
rm npbparams.h
#
chmod ugo+x a.out
mv a.out ~/binf77/ep_serial_S
#
echo "Executable installed as ~/binf77/ep_serial_S"
