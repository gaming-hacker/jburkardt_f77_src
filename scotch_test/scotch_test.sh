#! /bin/bash
#
gfortran -c -Wall scotch_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o scotch_test scotch_test.o -L/usr/lib/x86_64-linux-gnu -lscotch-6
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm scotch_test.o
#
#./scotch_test > scotch_test.txt
./scotch_test
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm scotch_test
#
echo "Normal end of execution."
