#!/bin/bash
#
gfortran -c high_card_simulation_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling high_card_simulation_prb.f"
  exit
fi
#
gfortran high_card_simulation_prb.o -L$HOME/libf77 -lhigh_card_simulation
if [ $? -ne 0 ]; then
  echo "Errors linking and loading high_card_simulation_prb.o"
  exit
fi
rm high_card_simulation_prb.o
#
mv a.out high_card_simulation_prb
./high_card_simulation_prb > high_card_simulation_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running high_card_simulation_prb"
  exit
fi
rm high_card_simulation_prb
#
echo "Test program output written to high_card_simulation_prb_output.txt."
