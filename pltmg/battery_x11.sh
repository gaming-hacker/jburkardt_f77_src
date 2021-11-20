#!/bin/bash
#
gfortran -c atest.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling atest.f"
  exit
fi
rm compiler.txt
#
gfortran -c battery.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling battery.f"
  exit
fi
rm compiler.txt
#
gfortran -c mgmpi_stubs.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling mgmpi_stubs.f"
  exit
fi
rm compiler.txt
#
gfortran -c mgvio_stubs.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling mgvio_stubs.f"
  exit
fi
rm compiler.txt
#
gcc -c xgui.c >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling xgui.c"
  exit
fi
rm compiler.txt
#
#
gfortran atest.o battery.o mgmpi_stubs.o mgvio_stubs.o xgui.o \
  -L$HOME/libf77 -lpltmg_double -lrpcsvc -L/usr/X11R6/lib -lXt -lXaw
if [ $? -ne 0 ]; then
  echo "Errors linking and loading "
  echo "  atest.o + battery.o + mgmpi_stubs.o + mgvio_stubs.o"
  exit
fi
rm atest.o
rm battery.o
rm mgmpi_stubs.o
rm mgvio_stubs.o
#
mv a.out battery_x11
mv battery_x11 $HOME/bin
#
echo "The pltmg battery_x11 test program has been created."
