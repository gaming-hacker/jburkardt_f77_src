#!/bin/bash
#
#  Compile
#
gfortran -c z_sample.f
if [ $? -ne 0 ]; then
  echo "Errors compiling z_sample.f"
  exit
fi
#
gcc -c -I/$HOME/include c_fortran_zgssv.c
if [ $? -ne 0 ]; then
  echo "Errors compiling c_fortran_zgssv.c"
  exit
fi
#
#  Link and load
#
gfortran z_sample.o c_fortran_zgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading z_sample.o + c_fortran_zgssv.o"
  exit
fi
rm z_sample.o
rm c_fortran_zgssv.o
mv a.out z_sample
#
#  Run
#
./z_sample > z_sample_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running z_sample"
  exit
fi
rm z_sample
#
#  Terminate.
#
echo "Program output written to z_sample_output.txt"
