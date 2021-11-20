#!/bin/bash
#
gfortran -c chebyshev_interp_1d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling chebyshev_interp_1d_prb.f"
  exit
fi
#
gfortran chebyshev_interp_1d_prb.o -L$HOME/libf77 -lchebyshev_interp_1d \
  -ltest_interp -lqr_solve -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading chebyshev_interp_1d_prb.o"
  exit
fi
rm chebyshev_interp_1d_prb.o
#
mv a.out chebyshev_interp_1d_prb
./chebyshev_interp_1d_prb > chebyshev_interp_1d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running chebyshev_interp_1d_prb"
  exit
fi
rm chebyshev_interp_1d_prb
#
echo "Test program output written to chebyshev_interp_1d_prb_output.txt."
