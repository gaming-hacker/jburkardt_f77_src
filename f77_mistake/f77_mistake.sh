#! /bin/bash
#
gfortran -c f77_mistake.f
if [ $? -ne 0 ]; then
  echo "Errors compiling f77_mistake.f"
  exit
fi
#
gfortran f77_mistake.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading f77_mistake.o"
  exit
fi
rm f77_mistake.o
#
mv a.out f77_mistake
./f77_mistake > f77_mistake_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running f77_mistake"
  exit
fi
rm f77_mistake
#
echo "Test results written to f77_mistake_output.txt."
