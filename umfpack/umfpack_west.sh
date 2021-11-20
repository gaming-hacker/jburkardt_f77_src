#!/bin/bash
#
#  Compile
#
gfortran -c umfpack_west.f
if [ $? -ne 0 ]; then
  echo "Errors compiling umfpack_west.f"
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
gfortran umfpack_west.o umf4_f77wrapper.o  -L/usr/local/lib -L/$HOME/lib \
  -lumfpack -lamd -lcholmod -lcolamd -lm -lsuitesparseconfig -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading umfpack_west.o"
  exit
fi
rm umfpack_west.o
rm umf4_f77wrapper.o
mv a.out umfpack_west
#
#  Run
#
./umfpack_west > umfpack_west_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running umfpack_west"
  exit
fi
rm umfpack_west
#
#  Terminate.
#
echo "Program output written to umfpack_west_output.txt"
