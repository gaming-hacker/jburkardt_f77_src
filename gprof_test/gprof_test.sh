#!/bin/bash
#
gfortran -c -pg linpack_bench.f
if [ $? -ne 0 ]; then
  echo "Errors compiling linpack_bench.f"
  exit
fi
#
gfortran -pg linpack_bench.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading linpack_bench.o"
  exit
fi
rm linpack_bench.o
#
mv a.out linpack_bench
#
./linpack_bench > linpack_bench.txt
#
gprof linpack_bench > gprof_test.txt
#
#  Clean up.
#  GPROF creates a temporary file GMON.OUT that we don't need.
#
rm linpack_bench
rm gmon.out
#
echo "Program output written to linpack_bench.txt"
echo "GPROF report written to gprof_test.txt"
