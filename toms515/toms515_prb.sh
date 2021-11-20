#! /bin/bash
#
gfortran -c toms515_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms515_prb.f"
  exit
fi
#
gfortran toms515_prb.o -L$HOME/libf77 -ltoms515
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms515_prb.o"
  exit
fi
rm toms515_prb.o
#
mv a.out toms515_prb
./toms515_prb > toms515_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms515_prb"
  exit
fi
rm toms515_prb
#
echo "Test results written to toms515_prb_output.txt."
