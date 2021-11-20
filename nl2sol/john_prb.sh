#!/bin/bash
#
gfortran john_prb.f -L$HOME/libf77 -lnl2sol
mv a.out john_prb
./john_prb
