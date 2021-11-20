#!/bin/bash
#
gfortran -c chrpak_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling chrpak_prb.f"
  exit
fi
#
gfortran chrpak_prb.o -L$HOME/libf77 -lchrpak
if [ $? -ne 0 ]; then
  echo "Errors linking and loading chrpak_prb.o"
  exit
fi
rm chrpak_prb.o
#
mv a.out chrpak_prb
./chrpak_prb > chrpak_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running chrpak_prb"
  exit
fi
rm chrpak_prb
#
echo "Test results written to chrpak_prb_output.txt."
