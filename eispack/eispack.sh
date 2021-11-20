#! /bin/bash
#
#! /bin/bash
#
gfortran -c -Wall eispack.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv eispack.o ~/libf77/eispack.o
#
echo "Normal end of execution."
