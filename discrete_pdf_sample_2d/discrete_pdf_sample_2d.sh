#!/bin/bash
#
gfortran -c discrete_pdf_sample_2d.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling discrete_pdf_sample_2d.f"
  exit
fi
#
gfortran discrete_pdf_sample_2d.o
if [ $? -ne 0 ]; then
  echo "Errors while loading discrete_pdf_sample_2d.o"
  exit
fi
rm discrete_pdf_sample_2d.o
#
mv a.out ~/binf77/discrete_pdf_sample_2d
#
echo "Executable installed as ~/binf77/discrete_pdf_sample_2d"
