#!/bin/bash
#
gfortran -c burgers_solution_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling burgers_solution_prb.f"
  exit
fi
#
gfortran burgers_solution_prb.o -L$HOME/libf77 -lburgers_solution
if [ $? -ne 0 ]; then
  echo "Errors linking and loading burgers_solution_prb.o"
  exit
fi
rm burgers_solution_prb.o
#
mv a.out burgers_solution_prb
./burgers_solution_prb > burgers_solution_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running burgers_solution_prb"
  exit
fi
rm burgers_solution_prb
#
echo "Program output written to burgers_solution_prb_output.txt"
