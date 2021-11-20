#!/bin/bash
#
gfortran -c newton_rc_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling newton_rc_prb.f"
  exit
fi
#
gfortran newton_rc_prb.o -L$HOME/libf77 -lnewton_rc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading newton_rc_prb.o"
  exit
fi
rm newton_rc_prb.o
#
mv a.out newton_rc_prb
./newton_rc_prb > newton_rc_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running newton_rc_prb"
  exit
fi
rm newton_rc_prb
#
echo "Test results written to newton_rc_prb_output.txt."
