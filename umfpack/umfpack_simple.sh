#!/bin/bash
#
#  Compile
#
gfortran -c umfpack_simple.f
if [ $? -ne 0 ]; then
  echo "Errors compiling umfpack_simple.f"
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
gfortran umfpack_simple.o umf4_f77wrapper.o  -L/usr/local/lib -L/$HOME/lib \
  -lumfpack -lamd -lcholmod -lcolamd -lm -lsuitesparseconfig -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading umfpack_simple.o"
  exit
fi
rm umfpack_simple.o
rm umf4_f77wrapper.o
mv a.out umfpack_simple
#
#  Run
#
./umfpack_simple > umfpack_simple_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running umfpack_simple"
  exit
fi
rm umfpack_simple
#
#  Terminate.
#
echo "Program output written to umfpack_simple_output.txt"
