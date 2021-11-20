#!/bin/bash
#
gfortran -c chebyshev_series_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling chebyshev_series_prb.f"
  exit
fi
#
gfortran chebyshev_series_prb.o -L$HOME/libf77 -lchebyshev_series
if [ $? -ne 0 ]; then
  echo "Errors linking and loading chebyshev_series_prb.o"
  exit
fi
rm chebyshev_series_prb.o
#
mv a.out chebyshev_series_prb
./chebyshev_series_prb > chebyshev_series_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running chebyshev_series_prb"
  exit
fi
rm chebyshev_series_prb
#
echo "Test program output written to chebyshev_series_prb_output.txt."
