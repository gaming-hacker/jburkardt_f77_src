#!/bin/csh
#
F77 -c sparsepak_fem.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling sparsepak_fem.f"
  exit
endif
rm compiler.txt
#
F77 sparsepak_fem.o ~/libf77/$ARCH/libsparsepak.a
if ( $status != 0 ) then
  echo "Errors linking sparsepak_fem.o"
  exit
endif
rm sparsepak_fem.o
#
mv a.out sparsepak_fem
sparsepak_fem > sparsepak_fem_output.txt
#
if ($status != 0) then
  echo "Errors running sparsepak_fem"
  exit
endif
#
rm sparsepak_fem
#
echo "Program output written to sparsepak_fem_output.txt"

