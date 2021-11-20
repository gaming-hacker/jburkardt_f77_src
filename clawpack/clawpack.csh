#!/bin/csh
#
echo "Split the file."
#
mkdir temp
cd temp
rm *
f77split ../clawpack.f
#
echo "Compile the routines."
#
foreach FILE (`ls -1 *.f`)
  g77 -c -g $FILE >& compiler.out
  if ( $status != 0 ) then
    echo "Errors compiling " $FILE
    exit
  endif
  rm compiler.out
end
rm *.f
#
echo "Create the archive."
ar qc libclawpack.a *.o
rm *.o
#
echo "Store the archive."
mv libclawpack.a ~/libf77/$ARCH
cd ..
rmdir temp
#
echo "A new version of clawpack has been created."
