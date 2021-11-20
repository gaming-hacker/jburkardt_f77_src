#!/bin/bash
#
cp ~/include/netcdf.inc .
gfortran -c sfc_pres_temp_rd.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sfc_pres_temp_rd.f"
  exit
fi
rm netcdf.inc
#
gfortran sfc_pres_temp_rd.o -L$HOME/lib -lnetcdf
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sfc_pres_temp_rd.o"
  exit
fi
rm sfc_pres_temp_rd.o
#
mv a.out sfc_pres_temp_rd
./sfc_pres_temp_rd > sfc_pres_temp_rd_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sfc_pres_temp_rd"
  exit
fi
rm sfc_pres_temp_rd
#
echo "Test results written to sfc_pres_temp_rd_output.txt."
