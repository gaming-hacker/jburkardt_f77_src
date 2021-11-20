#!/bin/bash
#
gfortran -c subpak_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling subpak_prb.f"
  exit
fi
#
gfortran subpak_prb.o -L$HOME/libf77 -lsubpak
if [ $? -ne 0 ]; then
  echo "Errors linking and loading subpak_prb.o"
  exit
fi
rm subpak_prb.o
#
mv a.out subpak_prb
./subpak_prb > subpak_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running subpak_prb"
  exit
fi
rm subpak_prb
#
echo "Test results written to subpak_prb_output.txt."
