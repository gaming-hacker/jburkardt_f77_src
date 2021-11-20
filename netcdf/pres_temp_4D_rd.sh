#!/bin/bash
#
cp ~/include/netcdf.inc .
gfortran -c pres_temp_4D_rd.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pres_temp_4D_rd.f"
  exit
fi
rm netcdf.inc
#
gfortran pres_temp_4D_rd.o -L$HOME/lib -lnetcdf
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pres_temp_4D_rd.o"
  exit
fi
rm pres_temp_4D_rd.o
#
mv a.out pres_temp_4D_rd
./pres_temp_4D_rd > pres_temp_4D_rd_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pres_temp_4D_rd"
  exit
fi
rm pres_temp_4D_rd
#
echo "Test results written to pres_temp_4D_rd_output.txt."
