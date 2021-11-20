#! /bin/bash
#
gfortran -c -Wall test_eigen_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_eigen_test.o -L$HOME/libf77 -ltest_eigen
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_eigen_test.o
#
mv a.out test_eigen_test
./test_eigen_test > test_eigen_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_eigen_test
#
echo "Normal end of execution."
