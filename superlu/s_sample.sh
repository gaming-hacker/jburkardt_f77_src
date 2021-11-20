#!/bin/bash
#
#  Compile
#
gfortran -c s_sample.f
if [ $? -ne 0 ]; then
  echo "Errors compiling s_sample.f"
  exit
fi
#
gcc -c -I/$HOME/include c_fortran_sgssv.c
if [ $? -ne 0 ]; then
  echo "Errors compiling c_fortran_sgssv.c"
  exit
fi
#
#  Link and load
#
gfortran s_sample.o c_fortran_sgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading s_sample.o + c_fortran_sgssv.o"
  exit
fi
rm s_sample.o
rm c_fortran_sgssv.o
mv a.out s_sample
#
#  Run
#
./s_sample > s_sample_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running s_sample"
  exit
fi
rm s_sample
#
#  Terminate.
#
echo "Program output written to s_sample_output.txt"
