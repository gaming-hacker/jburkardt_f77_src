#!/bin/bash
#
#  Compile
#
gfortran -c umfpack_wathen.f
if [ $? -ne 0 ]; then
  echo "Errors compiling umfpack_wathen.f"
  exit
fi
#
gcc -c umf4_f77wrapper.c
if [ $? -ne 0 ]; then
  echo "Errors compiling umf4_f77wrapper.c"
  exit
fi
#
#  Link and load
#
gfortran umfpack_wathen.o umf4_f77wrapper.o  -L/usr/local/lib -L/$HOME/lib \
  -lumfpack -lamd -lcholmod -lcolamd -lm -lsuitesparseconfig -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading umfpack_wathen.o"
  exit
fi
rm umfpack_wathen.o
rm umf4_f77wrapper.o
mv a.out umfpack_wathen
#
#  Run
#
./umfpack_wathen > umfpack_wathen_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running umfpack_wathen"
  exit
fi
rm umfpack_wathen
#
#  Terminate.
#
echo "Program output written to umfpack_wathen_output.txt"
