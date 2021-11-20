#! /bin/bash
#
gfortran -c -Wall upjode.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -c -Wall anynul.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran upjode.o anynul.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm upjode.o
rm anynul.o
#
chmod ugo+x a.out
mv a.out ~/binf77/upjode
#
echo "Normal end of execution."
