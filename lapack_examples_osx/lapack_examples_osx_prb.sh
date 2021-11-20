#!/bin/bash
#
gfortran -c lapack_examples_osx_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lapack_examples_osx_prb.f"
  exit
fi
#
gfortran lapack_examples_osx_prb.o -framework vecLib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lapack_examples_osx_prb.o"
  exit
fi
rm lapack_examples_osx_prb.o
#
mv a.out lapack_examples_osx_prb
./lapack_examples_osx_prb > lapack_examples_osx_prb_output.txt
rm lapack_examples_osx_prb
#
echo "Program output written to lapack_examples_osx_prb_output.txt"
