#!/bin/bash
#
gfortran -c hb_to_st.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hb_sparse_to_triplet.f"
  exit
fi
#
gfortran hb_to_st.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hb_to_st.o"
  exit
fi
rm hb_to_st.o
#
chmod ugo+x a.out
mv a.out ~/binf77/hb_to_st
#
echo "Executable installed as ~/binf77/hb_to_st"
