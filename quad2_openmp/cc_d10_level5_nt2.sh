#!/bin/bash
#
export OMP_NUM_THREADS=2
#
~/binf77/OSX/quad2_open_mp cc_d10_level5 > cc_d10_level5_nt2_output.txt
#
echo "Program output written to cc_d10_level5_nt2_output.txt"
