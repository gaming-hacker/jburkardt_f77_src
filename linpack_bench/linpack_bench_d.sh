#!/bin/bash
#
gfortran -c linpack_bench_d.f
if [ $? -ne 0 ]; then
  echo "Errors compiling linpack_bench_d.f"
  exit
fi
#
gfortran linpack_bench_d.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading linpack_bench_d.o"
  exit
fi
rm linpack_bench_d.o
#
mv a.out ~/binf77/linpack_bench_d
#
echo "Program installed as ~/binf77/linpack_bench_d"
