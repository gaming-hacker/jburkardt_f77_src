#!/bin/bash
#
gfortran -c image_edge_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling image_edge_prb.f"
  exit
fi
#
gfortran image_edge_prb.o -L$HOME/libf77 -limage_edge
if [ $? -ne 0 ]; then
  echo "Errors linking and loading image_edge_prb.o"
  exit
fi
rm image_edge_prb.o
#
mv a.out image_edge_prb
./image_edge_prb > image_edge_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running image_edge_prb"
  exit
fi
rm image_edge_prb
#
echo "Test results written to image_edge_prb_output.txt."
