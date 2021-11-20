#!/bin/csh
#
F77 -c -g bufpak_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling bufpak_prb.f"
  exit
endif
rm compiler.txt
#
F77 bufpak_prb.o -L$HOME/libf77/$ARCH -lbufpak
if ( $status != 0 ) then
  echo "Errors linking and loading bufpak_prb.o"
  exit
endif
rm bufpak_prb.o
#
mv a.out bufpak_prb
./bufpak_prb > bufpak_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running bufpak_prb"
  exit
endif
rm bufpak_prb
#
echo "Test results written to bufpak_prb_output.txt."
