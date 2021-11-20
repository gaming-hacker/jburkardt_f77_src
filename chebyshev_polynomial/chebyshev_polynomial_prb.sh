#!/bin/bash
#
gfortran -c chebyshev_polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling chebyshev_polynomial_prb.f"
  exit
fi
#
gfortran chebyshev_polynomial_prb.o -L$HOME/libf77 -lchebyshev_polynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading chebyshev_polynomial_prb.o"
  exit
fi
rm chebyshev_polynomial_prb.o
#
mv a.out chebyshev_polynomial_prb
./chebyshev_polynomial_prb > chebyshev_polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running chebyshev_polynomial_prb"
  exit
fi
rm chebyshev_polynomial_prb
#
echo "Program output written to chebyshev_polynomial_prb_output.txt"
