#!/bin/csh
#
mkdir temp
cd temp
rm *
~/binc/$ARCH/f77split ../bufpak.f
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
ar qc libbufpak.a *.o
rm *.o
#
mv libbufpak.a ~/libf77/$ARCH
cd ..
rmdir temp
#
echo "Library installed as ~/libf77/$ARCH/libbufpak.a."
