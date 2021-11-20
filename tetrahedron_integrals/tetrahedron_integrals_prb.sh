#!/bin/bash
#
gfortran -c tetrahedron_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tetrahedron_integrals_prb.f"
  exit
fi
#
gfortran tetrahedron_integrals_prb.o -L$HOME/libf77 -ltetrahedron_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tetrahedron_integrals_prb.o"
  exit
fi
rm tetrahedron_integrals_prb.o
#
mv a.out tetrahedron_integrals_prb
./tetrahedron_integrals_prb > tetrahedron_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running tetrahedron_integrals_prb"
  exit
fi
rm tetrahedron_integrals_prb
#
echo "Test program output written to tetrahedron_integrals_prb_output.txt."
