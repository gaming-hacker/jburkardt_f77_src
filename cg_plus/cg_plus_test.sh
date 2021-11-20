#! /bin/bash
#
gfortran -c -Wall cg_plus_test.f
if [ $? -ne 0 ]; then
  echo "Compile error.f"
  exit
fi
#
gfortran cg_plus_test.o -L$HOME/libf77 -lcg_plus
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cg_plus_test.o
#
mv a.out cg_plus_test
./cg_plus_test > cg_plus_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cg_plus_test
#
echo "Normal end of execution."
