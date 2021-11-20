#!/bin/csh
#
F77 -c -g flow_1d_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling flow_1d_prb.f"
  exit
endif
rm compiler.txt
#
F77 flow_1d_prb.o -L$HOME/libf77/$ARCH -lflow_1d
if ( $status != 0 ) then
  echo "Errors linking and loading flow_1d_prb.o"
  exit
endif
rm flow_1d_prb.o
#
mv a.out flow_1d_prb
./flow_1d_prb > flow_1d_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running flow_1d_prb"
  exit
endif
rm flow_1d_prb
#
echo "Test results written to flow_1d_prb_output.txt."
