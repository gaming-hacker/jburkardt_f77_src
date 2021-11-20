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
ar qc libbiharmonic_2d.a *.o
rm *.o
#
mv libbiharmonic_2d.a ~/libf77
#
echo "Normal end of execution."
