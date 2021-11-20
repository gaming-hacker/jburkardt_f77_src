#! /bin/bash
#
gfortran -c -Wall svd_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran svd_test.o -L$HOME/libf77 -llapack -llinpack_d -lblas
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm svd_test.o
#
mv a.out svd_test
mv svd_test $HOME/binf77
#
echo "Normal end of execution."
