#! /bin/bash
#
gfortran -c -Wall eispack_test.f
if [ $? -ne 0 ]; then
  echo "Compile errors."
  exit
fi
#
gfortran eispack_test.o $HOME/libf77/eispack.o
if [ $? -ne 0 ]; then
  echo "Load errors."
  exit
fi
rm eispack_test.o
#
mv a.out eispack_test
./eispack_test > eispack_test.txt
if [ $? -ne 0 ]; then
  echo "Run errors."
  exit
fi
rm eispack_test
#
echo "Normal end of execution."
