#! /bin/bash
#
gfortran -c spaeth2_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling spaeth2_prb.f"
  exit
fi
#
gfortran -o spaeth2_prb spaeth2_prb.o -L$HOME/libf77 -lspaeth2
if [ $? -ne 0 ]; then
  echo "Errors linking and loading spaeth2_prb.o"
  exit
fi
rm spaeth2_prb.o
#
./spaeth2_prb > spaeth2_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running spaeth2_prb"
  exit
fi
rm spaeth2_prb
#
echo "Test results written to spaeth2_prb_output.txt."
