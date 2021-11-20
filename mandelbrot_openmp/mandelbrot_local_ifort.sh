#!/bin/bash
#
#  Compile with IFORT.
#
ifort -openmp -parallel -fpp mandelbrot_open_mp.f
#
mv a.out mandelbrot
#
#  Request 1 thread.
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
./mandelbrot > mandelbrot_local_ifort_output.txt
#
#  Request 2 threads.
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
./mandelbrot >> mandelbrot_local_ifort_output.txt
#
#  Request 4 threads.
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
./mandelbrot >> mandelbrot_local_ifort_output.txt
#
#  Discard the executable.
#
rm mandelbrot
#
echo "Program output written to mandelbrot_local_ifort_output.txt"
