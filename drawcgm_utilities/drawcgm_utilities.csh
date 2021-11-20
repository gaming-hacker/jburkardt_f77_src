#!/bin/csh
#
mkdir temp
cd temp
rm *
~/binc/$ARCH/f77split ../drawcgm_utilities.f
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
ar qc libdrawcgm_utilities.a *.o
rm *.o
#
mv libdrawcgm_utilities.a ~/libf77/$ARCH
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/$ARCH/libdrawcgm_utilities.a."
