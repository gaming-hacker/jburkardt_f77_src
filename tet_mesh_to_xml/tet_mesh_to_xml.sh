#!/bin/bash
#
gfortran -c tet_mesh_to_xml.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling tet_mesh_to_xml.f"
  exit
fi
rm compiler.txt
#
gfortran tet_mesh_to_xml.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tet_mesh_to_xml.o"
  exit
fi
#
rm tet_mesh_to_xml.o
#
chmod ugo+x a.out
mv a.out ~/binf77/tet_mesh_to_xml
#
echo "Executable installed as ~/binf77/tet_mesh_to_xml"
