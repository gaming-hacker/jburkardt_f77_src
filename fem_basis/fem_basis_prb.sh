#!/bin/bash
#
gfortran -c fem_basis_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem_basis_prb.f"
  exit
fi
#
gfortran fem_basis_prb.o -L$HOME/libf77 -lfem_basis
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem_basis_prb.o"
  exit
fi
rm fem_basis_prb.o
#
mv a.out fem_basis_prb
./fem_basis_prb > fem_basis_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fem_basis_prb"
  exit
fi
rm fem_basis_prb
#
echo "Test program output written to fem_basis_prb_output.txt."
