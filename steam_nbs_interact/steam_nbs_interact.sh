#!/bin/bash
#
gfortran -c steam_nbs_interact.f
if [ $? -ne 0 ]; then
  echo "Errors compiling steam_nbs_interact.f"
  exit
fi
#
gfortran steam_nbs_interact.o -L$HOME/libf77 -lsteam_nbs
if [ $? -ne 0 ]; then
  echo "Errors linking and loading steam_nbs_interact.o"
  exit
fi
rm steam_nbs_interact.o
#
mv a.out ~/binf77/steam_nbs_interact
#
echo "Executable installed as ~/binf77/steam_nbs_interact"
