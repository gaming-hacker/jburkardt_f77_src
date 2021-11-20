#!/bin/bash
#
gfortran ../openmp/compute_pi.f -L$HOME/libf77 -lopenmp_stubs
mv a.out compute_pi
#
#  Run the program.
#
./compute_pi > compute_pi_output.txt
rm compute_pi
#
echo "Normal end of execution."
