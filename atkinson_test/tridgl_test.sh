#! /bin/bash
#
gfortran -c -Wall tridgl_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o tridgl_test tridgl_test.o -L$HOME/libf77 -latkinson
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tridgl_test.o
#
./tridgl_test > tridgl_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tridgl_test
#
echo "Normal end of execution."
