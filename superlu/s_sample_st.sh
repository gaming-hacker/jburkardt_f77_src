#!/bin/bash
#
#  Compile
#
gfortran -c s_sample_st.f
if [ $? -ne 0 ]; then
  echo "Errors compiling s_sample_st.f"
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
gfortran s_sample_st.o c_fortran_sgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_4.3 -lm -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading s_sample_st.o + c_fortran_sgssv.o"
  exit
fi
rm s_sample_st.o
rm c_fortran_sgssv.o
mv a.out s_sample_st
#
#  Run
#
./s_sample_st > s_sample_st_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running s_sample_st"
  exit
fi
rm s_sample_st
#
#  Terminate.
#
echo "Program output written to s_sample_st_output.txt"
