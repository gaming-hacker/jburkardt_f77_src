#! /bin/bash
#
gfortran -c disk01_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran disk01_integrals_prb.o -L$HOME/libf77 -ldisk01_integrals
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk01_integrals_prb.o
#
mv a.out disk01_integrals_prb
./disk01_integrals_prb > disk01_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk01_integrals_prb
#
echo "Normal end of execution."
