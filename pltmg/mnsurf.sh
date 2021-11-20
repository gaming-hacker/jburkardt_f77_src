#!/bin/bash
#
gfortran -c atest.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling atest.f"
  exit
fi
rm compiler.txt
#
gfortran -c mnsurf.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling mnsurf.f"
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
echo "Compile xgui_stubs.c"
gcc -c xgui_stubs.c >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling xgui_stubs.c"
  exit
fi
rm compiler.txt
#
gfortran atest.o mnsurf.o mgmpi_stubs.o mgvio_stubs.o xgui_stubs.o \
  -L$HOME/libf77 -lpltmg_double -lrpcsvc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading "
  echo "  atest.o + mnsurf.o + mgmpi_stubs.o + mgvio_stubs.o + xgui_stubs.o"
  exit
fi
rm atest.o
rm mnsurf.o
rm mgmpi_stubs.o
rm mgvio_stubs.o
rm xgui_stubs.o
#
mv a.out mnsurf
mv mnsurf $HOME/bin
#
echo "The pltmg mnsurf test program has been created."
