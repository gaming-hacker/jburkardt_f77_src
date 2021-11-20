#!/bin/bash
#
cp npbparams_W.h npbparams.h
#
gfortran -c mg_serial.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mg_serial.f"
  exit
fi
#
gfortran -c print_results.f
if [ $? -ne 0 ]; then
  echo "Errors compiling print_results.f"
  exit
fi
#
gfortran -c randdp.f
if [ $? -ne 0 ]; then
  echo "Errors compiling randdp.f"
  exit
fi
#
gfortran -c timers.f
if [ $? -ne 0 ]; then
  echo "Errors compiling timers.f"
  exit
fi
#
gcc -c wtime.c
if [ $? -ne 0 ]; then
  echo "Errors compiling wtime.c"
  exit
fi
#
gfortran mg_serial.o print_results.o randdp.o timers.o wtime.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mg_serial.o + print_results.o + randdp.o + timers.o + wtime.o"
  exit
fi
rm mg_serial.o
rm print_results.o
rm randdp.o
rm timers.o
rm wtime.o
rm npbparams.h
#
chmod ugo+x a.out
mv a.out ~/binf77/mg_serial_W
#
echo "Executable installed as ~/binf77/mg_serial_W"
