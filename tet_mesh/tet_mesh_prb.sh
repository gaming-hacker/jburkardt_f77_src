#!/bin/bash
#
gfortran -c tet_mesh_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tet_mesh_prb.f"
  exit
fi
#
gfortran tet_mesh_prb.o -L$HOME/libf77 -ltet_mesh
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tet_mesh_prb.o"
  exit
fi
rm tet_mesh_prb.o
#
mv a.out tet_mesh_prb
./tet_mesh_prb > tet_mesh_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running tet_mesh_prb"
  exit
fi
rm tet_mesh_prb
#
echo "Test program output written to tet_mesh_prb_output.txt."
