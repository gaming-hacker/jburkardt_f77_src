#!/bin/bash
#
gfortran -c fem_to_xml.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem_to_xml.f"
  exit
fi
#
gfortran fem_to_xml.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem_to_xml.o"
  exit
fi
#
rm fem_to_xml.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem_to_xml
#
echo "Executable installed as ~/binf77/fem_to_xml"
