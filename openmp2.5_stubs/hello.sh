#!/bin/bash
#
gfortran ../openmp/hello.f -L$HOME/libf77 -lopenmp_stubs
mv a.out hello
#
#  Run the program.
#
./hello > hello_output.txt
rm hello
#
echo "Normal end of execution."
