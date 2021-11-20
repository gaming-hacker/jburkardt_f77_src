#!/bin/bash
#
#  Compile
#
gfortran -c d_sample_st.f
if [ $? -ne 0 ]; then
  echo "Errors compiling d_sample_st.f"
  exit
fi
#
gcc -c -I/$HOME/include c_fortran_dgssv.c
if [ $? -ne 0 ]; then
  echo "Errors compiling c_fortran_dgssv.c"
  exit
fi
#
#  Link and load
#
gfortran d_sample_st.o c_fortran_dgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading d_sample_st.o + c_fortran_dgssv.o"
  exit
fi
rm d_sample_st.o
rm c_fortran_dgssv.o
mv a.out d_sample_st
#
#  Run
#
./d_sample_st > d_sample_st_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running d_sample_st"
  exit
fi
rm d_sample_st
#
#  Terminate.
#
echo "Program output written to d_sample_st_output.txt"
