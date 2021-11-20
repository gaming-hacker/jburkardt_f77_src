#!/bin/bash
#
#  Compile with OpenMP.
#
gfortran -fopenmp quad2_openmp.f
#
mv a.out quad2_openmp
mv quad2_openmp ~/binf77
#
echo "Program installed as ~/binf77/quad2_openmp."
