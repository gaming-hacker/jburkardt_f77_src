#!/bin/bash
#
gfortran ../md_openmp/md_openmp.f -L$HOME/libf77 -lopenmp_stubs
mv a.out md
#
#  Run the program.
#
./md > md_output.txt
rm md
#
echo "Normal end of execution."
