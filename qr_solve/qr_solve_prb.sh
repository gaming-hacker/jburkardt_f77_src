#! /bin/bash
#
gfortran -c qr_solve_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling qr_solve_prb.f"
  exit
fi
#
gfortran qr_solve_prb.o -L$HOME/libf77 -lqr_solve -ltest_lls -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading qr_solve_prb.o"
  exit
fi
rm qr_solve_prb.o
#
mv a.out qr_solve_prb
./qr_solve_prb > qr_solve_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running qr_solve_prb"
  exit
fi
rm qr_solve_prb
#
echo "Test results written to qr_solve_prb_output.txt."
