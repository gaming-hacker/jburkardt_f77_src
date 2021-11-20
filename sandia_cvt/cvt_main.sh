#!/bin/bash
#
gfortran -c cvt_main.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cvt_main.f"
  exit
fi
#
gfortran cvt_main.o -L$HOME/libf77 -lsandia_cvt
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cvt_main.o"
  exit
fi
rm cvt_main.o
#
mv a.out ~/binf77/cvt_main
#
echo "Executable installed as ~/binf77/cvt_main"
