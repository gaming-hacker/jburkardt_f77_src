#!/bin/bash
#
gfortran -c latin_random_dataset.f
if [ $? -ne 0 ]; then
  echo "Errors compiling latin_random_dataset.f"
  exit
fi
#
gfortran latin_random_dataset.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading latin_random_dataset.o"
  exit
fi
rm latin_random_dataset.o
#
chmod ugo+x a.out
mv a.out ~/binf77/latin_random_dataset
#
echo "Executable installed as ~/binf77/latin_random_dataset"
