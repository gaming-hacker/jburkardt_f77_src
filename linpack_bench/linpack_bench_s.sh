#!/bin/bash
#
gfortran -c linpack_bench_s.f
if [ $? -ne 0 ]; then
  echo "Errors compiling linpack_bench_s.f"
  exit
fi
#
gfortran linpack_bench_s.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading linpack_bench_s.o"
  exit
fi
rm linpack_bench_s.o
#
mv a.out ~/binf77/linpack_bench_s
#
echo "Program installed as ~/binf77/linpack_bench_s"
