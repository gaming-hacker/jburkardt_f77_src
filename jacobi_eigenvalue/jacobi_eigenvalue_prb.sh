#!/bin/bash
#
gfortran -c jacobi_eigenvalue_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling jacobi_eigenvalue_prb.f"
  exit
fi
#
gfortran jacobi_eigenvalue_prb.o -L$HOME/libf77 -ljacobi_eigenvalue
if [ $? -ne 0 ]; then
  echo "Errors linking and loading jacobi_eigenvalue_prb.o"
  exit
fi
rm jacobi_eigenvalue_prb.o
#
mv a.out jacobi_eigenvalue_prb
./jacobi_eigenvalue_prb > jacobi_eigenvalue_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running jacobi_eigenvalue_prb"
  exit
fi
rm jacobi_eigenvalue_prb
#
echo "Test program output written to jacobi_eigenvalue_prb_output.txt."
