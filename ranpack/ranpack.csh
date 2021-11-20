#!/bin/csh
#  ranpack.csh  20 February 1999
#
setenv DEBUG FALSE
set echo
#
cd ~/src/ranpack
mkdir temp
cd temp
fsplit ../ranpack.f
fsplit ../ranpack_sgi.f
#
foreach FILE (`ls *.f`)
#
  if ( $DEBUG == TRUE ) then
    f77 -c -C -static -trapuv -u -w0 $FILE
    if ( $status != 0 ) exit
  else
    f77 -c $FILE
    if ( $status != 0 ) exit
  endif
  rm $FILE
end
#
ar qc libranpack.a *.o
rm *.o
#
mv libranpack.a ~/lib/$ARCH
cd ~/src/ranpack
rmdir temp
#
echo "A new version of ranpack has been created."
