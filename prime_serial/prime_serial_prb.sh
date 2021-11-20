#!/bin/bash
#
gfortran -c prime_serial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling prime_serial_prb.f"
  exit
fi
#
gfortran prime_serial_prb.o -L$HOME/libf77 -lprime_serial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading prime_serial_prb.o"
  exit
fi
rm prime_serial_prb.o
#
mv a.out prime_serial_prb
./prime_serial_prb > prime_serial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running prime_serial_prb"
  exit
fi
rm prime_serial_prb
#
echo "Test results written to prime_serial_prb_output.txt."
