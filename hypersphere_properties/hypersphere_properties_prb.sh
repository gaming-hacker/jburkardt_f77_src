#!/bin/bash
#
gfortran -c hypersphere_properties_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypersphere_properties_prb.f"
  exit
fi
#
gfortran hypersphere_properties_prb.o -L$HOME/libf77 -lhypersphere_properties
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypersphere_properties_prb.o"
  exit
fi
rm hypersphere_properties_prb.o
#
mv a.out hypersphere_properties_prb
./hypersphere_properties_prb > hypersphere_properties_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hypersphere_properties_prb"
  exit
fi
rm hypersphere_properties_prb
#
echo "Test program output written to hypersphere_properties_prb_output.txt."
