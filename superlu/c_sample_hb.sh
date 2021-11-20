#!/bin/bash
#
#  Compile
#
gfortran -c c_sample_hb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling c_sample_hb.f"
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
gfortran c_sample_hb.o c_fortran_cgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading c_sample_hb.o + c_fortran_cgssv.o"
  exit
fi
rm c_sample_hb.o
rm c_fortran_cgssv.o
mv a.out c_sample_hb
#
#  Run
#
./c_sample_hb < sample_cua.txt > c_sample_hb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running c_sample_hb"
  exit
fi
rm c_sample_hb
#
#  Terminate.
#
echo "Program output written to c_sample_hb_output.txt"
