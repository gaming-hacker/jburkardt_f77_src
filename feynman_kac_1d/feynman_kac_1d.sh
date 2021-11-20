#!/bin/bash
#
gfortran -c feynman_kac_1d.f
if [ $? -ne 0 ]; then
  echo "Errors compiling feynman_kac_1d.f"
  exit
fi
#
gfortran feynman_kac_1d.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading feynman_kac_1d.o"
  exit
fi
rm feynman_kac_1d.o
#
chmod ugo+x a.out
mv a.out feynman_kac_1d
./feynman_kac_1d > feynman_kac_1d_output.txt
rm feynman_kac_1d
#
echo "Program output written to feynman_kac_1d_output.txt"
