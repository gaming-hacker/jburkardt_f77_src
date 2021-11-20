#! /bin/bash
#
gfortran -c machine_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling machine_prb.f"
  exit
fi
#
gfortran machine_prb.o -L$HOME/libf77 -lmachine
if [ $? -ne 0 ]; then
  echo "Errors linking and loading machine_prb.o"
  exit
fi
rm machine_prb.o
#
mv a.out machine_prb
./machine_prb > machine_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running machine_prb"
  exit
fi
rm machine_prb
#
echo "Test results written to machine_prb_output.txt."
