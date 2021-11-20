#!/bin/bash
#
#  Compile
#
gfortran -c c_sample.f
if [ $? -ne 0 ]; then
  echo "Errors compiling c_sample.f"
  exit
fi
#
gcc -c -I/$HOME/include c_fortran_cgssv.c
if [ $? -ne 0 ]; then
  echo "Errors compiling c_fortran_cgssv.c"
  exit
fi
#
#  Link and load
#
gfortran c_sample.o c_fortran_cgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading c_sample.o + c_fortran_cgssv.o"
  exit
fi
rm c_sample.o
rm c_fortran_cgssv.o
mv a.out c_sample
#
#  Run
#
./c_sample > c_sample_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running c_sample"
  exit
fi
rm c_sample
#
#  Terminate.
#
echo "Program output written to c_sample_output.txt"
