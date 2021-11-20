#!/bin/bash
#
gfortran -c ns3de_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ns3de_prb.f"
  exit
fi
#
gfortran -o ns3de_prb ns3de_prb.o -L$HOME/libf77 -lns3de
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ns3de_prb.o"
  exit
fi
rm ns3de_prb.o
#
./ns3de_prb > ns3de_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ns3de_prb"
  exit
fi
rm ns3de_prb
#
echo "Test program output written to ns3de_prb_output.txt."
