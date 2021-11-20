#!/bin/csh
#
mkdir temp
cd temp
rm *
~/binc/$ARCH/f77split ../flow_1d.f
#
foreach FILE (`ls -1 *.f`)
  F77 -c -g $FILE >& compiler.txt
  if ( $status != 0 ) then
    echo "Errors compiling " $FILE
    exit
  endif
  rm compiler.txt
end
rm *.f
#
ar qc libflow_1d.a *.o
rm *.o
#
mv libflow_1d.a ~/libf77/$ARCH
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/$ARCH/libflow_1d.a."
