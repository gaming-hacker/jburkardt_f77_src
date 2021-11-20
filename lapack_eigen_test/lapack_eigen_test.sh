#! /bin/bash
#
gfortran -c -Wall lapack_eigen_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lapack_eigen_test.o -L$HOME/libf77/$ARCH -llapack -lblas -ltest_eigen
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lapack_eigen_test.o
#
mv a.out lapack_eigen_test
./lapack_eigen_test > lapack_eigen_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lapack_eigen_test
#
echo "Normal end of execution."
