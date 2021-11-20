#!/bin/bash
#
gfortran -c file_name_sequence_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling file_name_sequence_prb.f"
  exit
fi
#
gfortran file_name_sequence_prb.o -L$HOME/libf77 -lfile_name_sequence
if [ $? -ne 0 ]; then
  echo "Errors linking and loading file_name_sequence_prb.o"
  exit
fi
rm file_name_sequence_prb.o
#
mv a.out file_name_sequence_prb
./file_name_sequence_prb > file_name_sequence_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running file_name_sequence_prb"
  exit
fi
rm file_name_sequence_prb
#
echo "Test results written to file_name_sequence_prb_output.txt."
