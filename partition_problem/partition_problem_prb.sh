#!/bin/bash
#
gfortran -c partition_problem_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling partition_problem_prb.f"
  exit
fi
#
gfortran partition_problem_prb.o -L$HOME/libf77 -lpartition_problem
if [ $? -ne 0 ]; then
  echo "Errors linking and loading partition_problem_prb.o"
  exit
fi
rm partition_problem_prb.o
#
mv a.out partition_problem_prb
./partition_problem_prb > partition_problem_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running partition_problem_prb"
  exit
fi
rm partition_problem_prb
#
echo "Test program output written to partition_problem_prb_output.txt."
