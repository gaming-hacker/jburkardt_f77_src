#! /bin/bash
#
gfortran -c machar_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling machar_prb.f"
  exit
fi
#
gfortran machar_prb.o -L$HOME/libf77 -lmachar
if [ $? -ne 0 ]; then
  echo "Errors linking and loading machar_prb.o"
  exit
fi
rm machar_prb.o
#
mv a.out machar_prb
./machar_prb > machar_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running machar_prb"
  exit
fi
rm machar_prb
#
echo "Test results written to machar_prb_output.txt."
