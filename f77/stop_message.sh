#! /bin/bash
#
gfortran -c stop_message.f
if [ $? -ne 0 ]; then
  echo "Errors compiling stop_message.f"
  exit
fi
#
gfortran stop_message.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading stop_message.o"
  exit
fi
rm stop_message.o
#
mv a.out stop_message
./stop_message > stop_message_output.txt
rm stop_message
#
echo "Normal end of execution."
