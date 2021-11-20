#!/bin/bash
#
gfortran -c triangulation_node_to_element.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangulation_node_to_element.f"
  exit
fi
#
gfortran triangulation_node_to_element.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangulation_node_to_element.o"
  exit
fi
#
rm triangulation_node_to_element.o
#
chmod ugo+x a.out
mv a.out ~/binf77/triangulation_node_to_element
#
echo "Executable installed as ~/binf77/triangulation_node_to_element"
