#! /bin/bash
#
gfortran -c -Wall zero_rc_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran zero_rc_test.o $HOME/libf77/zero_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm zero_rc_test.o
#
mv a.out zero_rc_test
./zero_rc_test > zero_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm zero_rc_test
#
echo "Normal end of execution."
