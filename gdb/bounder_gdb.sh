#! /bin/bash
#
gfortran -g bounder.f
mv a.out bounder
gdb bounder < bounder_gdb_input.txt > bounder_gdb_output.txt
#
#  Discard the executable.
#
rm bounder
#
echo "Program output written to bounder_gdb_output.txt"
