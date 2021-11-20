#! /bin/bash
#
gfortran -c -Wall ex2p4.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ex2p4 ex2p4.o -L$HOME/libf77 -lconte_deboor
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ex2p4.o
#
./ex2p4 > ex2p4.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ex2p4
#
echo "Normal end of execution."
