#!/bin/bash
#
export OMP_NUM_THREADS=4
#
~/binf77/OSX/quad2_open_mp cc_d10_level5 > cc_d10_level5_nt4_output.txt
#
echo "Program output written to cc_d10_level5_nt4_output.txt"
