#! /bin/bash
#
gfortran -c histogram_data_2d_sample.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling histogram_data_2d_sample.f"
  exit
fi
#
gfortran histogram_data_2d_sample.o
if [ $? -ne 0 ]; then
  echo "Errors while loading histogram_data_2d_sample.o"
  exit
fi
rm histogram_data_2d_sample.o
#
mv a.out ~/binf77/histogram_data_2d_sample
#
echo "Executable installed as ~/binf77/histogram_data_2d_sample"
