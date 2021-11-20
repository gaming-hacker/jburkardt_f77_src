#! /bin/bash
#
gfortran -c -Wall jacobi_eigenvalue.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv jacobi_eigenvalue.o ~/libf77/jacobi_eigenvalue.o
#
echo "Normal end of execution."
