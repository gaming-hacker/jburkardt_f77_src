#! /bin/bash
#
for FILE in `ls -1 *.f`;
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
#
ar qc libatkinson.a *.o
rm *.o
#
mv libatkinson.a ~/libf77
#
echo "Normal end of execution."
