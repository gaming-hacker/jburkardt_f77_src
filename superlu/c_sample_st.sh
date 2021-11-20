#!/bin/bash
#
#  Compile
#
gfortran -c c_sample_st.f
if [ $? -ne 0 ]; then
  echo "Errors compiling c_sample_st.f"
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
gfortran c_sample_st.o c_fortran_cgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading c_sample_st.o + c_fortran_cgssv.o"
  exit
fi
rm c_sample_st.o
rm c_fortran_cgssv.o
mv a.out c_sample_st
#
#  Run
#
./c_sample_st > c_sample_st_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running c_sample_st"
  exit
fi
rm c_sample_st
#
#  Terminate.
#
echo "Program output written to c_sample_st_output.txt"
