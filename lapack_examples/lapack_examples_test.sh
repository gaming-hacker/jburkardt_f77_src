#! /bin/bash
#
gfortran -c -Wall lapack_examples_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lapack_examples_test.o -L$HOME/libf77 -llapack -lblas
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lapack_examples_test.o
#
mv a.out lapack_examples_test
./lapack_examples_test > lapack_examples_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lapack_examples_test
#
echo "Normal end of execution."
