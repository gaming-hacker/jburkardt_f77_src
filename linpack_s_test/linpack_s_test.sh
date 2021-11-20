#! /bin/bash
#
gfortran -c -Wall linpack_s_test.f
if [ $? -ne 0 ]; then
  echo "Errors compiling linpack_s_test.f"
  exit
fi
#
gfortran linpack_s_test.o -L$HOME/libf77 -llinpack_s -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading linpack_s_test.o"
  exit
fi
rm linpack_s_test.o
#
mv a.out linpack_s_test
./linpack_s_test > linpack_s_test.txt
if [ $? -ne 0 ]; then
  echo "Errors running linpack_s_test"
  exit
fi
rm linpack_s_test
#
echo "Normal end of execution."
